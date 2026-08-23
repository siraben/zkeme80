/* Annotate zkeme80 colon definitions in Ghidra.
 *
 * Returning Forth colon words start with `call docol` followed by a
 * thread of 16-bit little-endian code-field addresses terminated by
 * EXIT.  Non-returning definitions such as ABORT are skipped.
 * This script resolves each pointer back to its dictionary name (via
 * the labelmap JSON) and writes:
 *   - a plate comment on the word holding the whole thread, e.g.
 *       FORGET: WORD FIND DUP @ LATEST ! DP ! EXIT
 *   - end-of-line comments on every thread cell
 *   - memory references from each cell to its target, so XREFs and
 *     call graphs work through NEXT-based threading
 *
 * Usage: Zkeme80AnnotateForth.java <labelmap.json>
 *
 * @category zkeme80
 * @menupath Tools.zkeme80.Annotate Forth threads
 */
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.CodeUnit;
import ghidra.program.model.listing.CommentType;
import ghidra.program.model.listing.Listing;
import ghidra.program.model.mem.Memory;
import ghidra.program.model.symbol.RefType;
import ghidra.program.model.symbol.ReferenceManager;
import ghidra.program.model.symbol.SourceType;

public class Zkeme80AnnotateForth extends GhidraScript {

	private static final int MAX_ENTRIES = 512;

	private Map<Long, JsonObject> words = new HashMap<>();
	private Map<Long, String> names = new TreeMap<>();
	private Listing listing;
	private ReferenceManager refmgr;

	@Override
	public void run() throws Exception {
		listing = currentProgram.getListing();
		refmgr = currentProgram.getReferenceManager();
		String[] args = getScriptArgs();
		String path;
		if (args.length > 0) {
			path = args[0];
		} else {
			path = askFile("zkeme80 labelmap JSON", "Open").getAbsolutePath();
		}
		JsonObject data = JsonParser.parseString(
			new String(Files.readAllBytes(Paths.get(path)))).getAsJsonObject();

		Long docol = null;
		Long exitCfa = null;
		// docol is a bare assembler label, not a dictionary entry.
		for (JsonElement el : data.getAsJsonArray("labels")) {
			JsonObject lab = el.getAsJsonObject();
			if ("docol".equals(lab.get("name").getAsString())) {
				docol = lab.get("addr").getAsLong();
			}
		}
		JsonArray arr = data.getAsJsonArray("forth_words");
		for (JsonElement el : arr) {
			JsonObject w = el.getAsJsonObject();
			long addr = w.get("addr").getAsLong();
			words.putIfAbsent(addr, w);
			names.putIfAbsent(addr, w.get("name").getAsString());
			if ("EXIT".equals(w.get("name").getAsString()) && exitCfa == null) {
				exitCfa = addr;
			}
		}
		if (docol == null || exitCfa == null) {
			println("docol or EXIT not found in labelmap; nothing to do");
			return;
		}

		Memory mem = currentProgram.getMemory();
		int annotated = 0;
		for (long a : names.keySet()) {
			byte[] head = new byte[3];
			int got = mem.getBytes(toAddr(a), head);
			if (got != 3) {
				continue;
			}
			if ((head[0] & 0xff) != 0xCD) {
				continue;
			}
			long target = (head[1] & 0xff) | ((head[2] & 0xff) << 8);
			if (target != docol) {
				continue;
			}

			StringBuilder sb = new StringBuilder();
			boolean ok = true;
			long p = a + 3;
			boolean terminated = false;
			for (int i = 0; i < MAX_ENTRIES; i++) {
				byte[] raw = new byte[2];
				if (mem.getBytes(toAddr(p), raw) != 2) {
					ok = false;
					break;
				}
				long val = (raw[0] & 0xff) | ((raw[1] & 0xff) << 8);
				String nm = names.get(val);
				if ("EXIT".equals(nm)) {
					sb.append("EXIT");
					annotateCell(p, val, "EXIT");
					terminated = true;
					break;
				}
				if (nm == null) {
					ok = false;
					break;
				}

				sb.append(nm);
				annotateCell(p, val, nm);
				p += 2;

				if (isCellOperandWord(nm)) {
					Long operand = readU16(mem, p);
					if (operand == null) {
						ok = false;
						break;
					}
					sb.append("(").append(String.format("0x%04x", operand)).append(")");
					annotateOperand(p, String.format("operand 0x%04x", operand));
					p += 2;
				} else if ("LITSTRING".equals(nm)) {
					Long length = readU16(mem, p);
					if (length == null || length > 0x4000) {
						ok = false;
						break;
					}
					sb.append("(").append(length).append(" bytes)");
					annotateOperand(p, "string length " + length);
					// Length cell, payload, and its trailing NUL.  Threads need
					// not be aligned, so the next CFA may be at an odd address.
					p += 2 + length + 1;
				}
				sb.append(' ');
			}
			if (!ok || !terminated || sb.length() == 0) {
				continue;
			}

			CodeUnit baseCu = listing.getCodeUnitContaining(toAddr(a));
			if (baseCu != null) {
				baseCu.setComment(CommentType.PLATE,
					names.get(a) + ": " + sb.toString().trim());
			}
			annotated++;
		}
		println("Annotated " + annotated + " colon definitions.");
	}

	private boolean isCellOperandWord(String name) {
		return "LIT".equals(name) || "(')".equals(name) ||
			"BRANCH".equals(name) || "0BRANCH".equals(name) ||
			"JUMP".equals(name) || "0JUMP".equals(name);
	}

	private Long readU16(Memory mem, long addr) throws Exception {
		byte[] raw = new byte[2];
		if (mem.getBytes(toAddr(addr), raw) != 2) {
			return null;
		}
		return (long) ((raw[0] & 0xff) | ((raw[1] & 0xff) << 8));
	}

	private void annotateOperand(long addr, String text) {
		CodeUnit cu = listing.getCodeUnitContaining(toAddr(addr));
		if (cu != null) {
			cu.setComment(CommentType.EOL, text);
		}
	}

	private void annotateCell(long cellAddr, long target, String name) {
		Address ca = toAddr(cellAddr);
		CodeUnit cu = listing.getCodeUnitContaining(ca);
		if (cu != null) {
			cu.setComment(CommentType.EOL, name);
		}
		if (refmgr.getReferencesFrom(ca).length == 0) {
			refmgr.addMemoryReference(ca, toAddr(target), RefType.DATA,
				SourceType.USER_DEFINED, 0);
		}
	}
}
