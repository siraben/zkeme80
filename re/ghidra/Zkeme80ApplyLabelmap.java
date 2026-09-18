/* Apply the zkeme80 labelmap to a Ghidra program.
 *
 * Expects zkeme80.rom imported flat (base 0x0) with the z80 processor.
 * Headless:
 *   -postScript Zkeme80ApplyLabelmap.java src/zkeme80.ram-labelmap.json
 * Interactive: run from Script Manager; a file picker appears when no
 * argument is given.
 *
 * @category zkeme80
 * @menupath Tools.zkeme80.Apply labelmap
 */
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import ghidra.app.cmd.function.CreateFunctionCmd;
import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.symbol.SourceType;
import ghidra.program.model.symbol.Symbol;
import ghidra.program.model.symbol.SymbolTable;

public class Zkeme80ApplyLabelmap extends GhidraScript {

	private String sanitize(String name) {
		StringBuilder sb = new StringBuilder();
		for (char ch : name.toCharArray()) {
			if (Character.isLetterOrDigit(ch) || ch == '.' || ch == '_') {
				sb.append(ch);
			} else if (ch == '/') {
				sb.append("_per_");
			} else {
				sb.append('_');
			}
		}
		String s = sb.toString();
		if (s.isEmpty() || !(Character.isLetter(s.charAt(0)) || s.charAt(0) == '.' || s.charAt(0) == '_')) {
			s = "w_" + s;
		}
		return s.toUpperCase();
	}

	private String uniqueName(String base, Set<String> used) {
		String name = base;
		int n = 2;
		while (used.contains(name)) {
			name = base + "_" + n;
			n++;
		}
		used.add(name);
		return name;
	}

	@Override
	public void run() throws Exception {
		String[] args = getScriptArgs();
		String path;
		if (args.length > 0) {
			path = args[0];
		} else {
			File f = askFile("zkeme80 labelmap JSON", "Open");
			path = f.getAbsolutePath();
		}
		JsonObject data = JsonParser.parseString(
			new String(Files.readAllBytes(Paths.get(path)))).getAsJsonObject();

		SymbolTable symtab = currentProgram.getSymbolTable();
		Set<String> used = new HashSet<>();

		int nLabels = 0;
		JsonArray labels = data.getAsJsonArray("labels");
		for (JsonElement el : labels) {
			JsonObject lab = el.getAsJsonObject();
			Address a = toAddr(lab.get("addr").getAsLong());
			String prefix = "ram".equals(lab.has("region") ? lab.get("region").getAsString() : "")
					? "ram_"
					: "";
			String name = uniqueName(prefix + sanitize(lab.get("name").getAsString()), used);
			try {
				symtab.createLabel(a, name, SourceType.USER_DEFINED);
				nLabels++;
			} catch (Exception e) {
				println("label " + lab.get("name").getAsString() + " @ " +
					lab.get("addr_hex").getAsString() + " failed: " + e);
			}
		}

		int nFns = 0;
		Set<Long> seen = new HashSet<>();
		// Kernel labels and Forth words frequently share an address.  Keep
		// the assembler label as the function name and create each address
		// only once.
		for (JsonElement el : labels) {
			JsonObject lab = el.getAsJsonObject();
			if ("ram".equals(lab.has("region") ? lab.get("region").getAsString() : "")) {
				continue;
			}
			long raw = lab.get("addr").getAsLong();
			if (!seen.add(raw)) {
				continue;
			}
			nFns += tryCreateFunction(toAddr(raw));
		}
		JsonArray words = data.getAsJsonArray("forth_words");
		if (words != null) {
			for (JsonElement el : words) {
				JsonObject w = el.getAsJsonObject();
				long raw = w.get("addr").getAsLong();
				if (!seen.add(raw)) {
					continue;
				}
				nFns += tryCreateFunction(toAddr(raw));
			}
		}

		println("Applied " + nLabels + " labels, created " + nFns + " functions.");
	}

	private int tryCreateFunction(Address a) {
		Function existing = currentProgram.getFunctionManager().getFunctionContaining(a);
		if (existing != null) {
			return 0;
		}
		Instruction instr = currentProgram.getListing().getInstructionAt(a);
		if (instr == null) {
			// Auto-analysis may not have reached here (no entry points
			// from a flat ROM import); force one instruction so flows
			// can propagate during post-analysis.
			disassemble(a);
			instr = currentProgram.getListing().getInstructionAt(a);
			if (instr == null) {
				return 0; // genuinely data
			}
		}
		Symbol primary = currentProgram.getSymbolTable().getPrimarySymbol(a);
		String name = primary != null ? primary.getName() : null;
		CreateFunctionCmd cmd =
			new CreateFunctionCmd(name, a, null, SourceType.USER_DEFINED);
		return cmd.applyTo(currentProgram, monitor) ? 1 : 0;
	}
}
