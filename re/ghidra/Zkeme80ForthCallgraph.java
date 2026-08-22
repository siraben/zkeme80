/* Emit the static call graph between zkeme80 Forth words / kernel
 * routines as CSV: caller,callee,kind,count.
 *
 * Every instruction in page 0 is attributed to the nearest preceding
 * named routine (the same roll-up the retail-OS coverage analyzer
 * performs).  Edges come from Ghidra CALL flows (machine-code words
 * calling each other, e.g. `call dup`) and from DATA references
 * planted by Zkeme80AnnotateForth.java on threaded code cells.
 *
 * Usage: Zkeme80ForthCallgraph.java <labelmap.json> [out.csv]
 *
 * @category zkeme80
 * @menupath Tools.zkeme80.Forth call graph
 */
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import ghidra.app.script.GhidraScript;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Instruction;
import ghidra.program.model.symbol.Reference;

public class Zkeme80ForthCallgraph extends GhidraScript {

	private ghidra.program.model.listing.Listing listing;

	private static class Entry {
		long addr;
		String name;

		Entry(long addr, String name) {
			this.addr = addr;
			this.name = name;
		}
	}

	private List<Entry> segs = new ArrayList<>(); // sorted by addr

	private String owner(long pc) {
		int lo = 0;
		int hi = segs.size() - 1;
		String best = null;
		while (lo <= hi) {
			int mid = (lo + hi) >>> 1;
			if (segs.get(mid).addr <= pc) {
				best = segs.get(mid).name;
				lo = mid + 1;
			} else {
				hi = mid - 1;
			}
		}
		return best != null ? best : ("unk_" + Long.toHexString(pc));
	}

	@Override
	public void run() throws Exception {
		listing = currentProgram.getListing();
		String[] args = getScriptArgs();
		if (args.length < 1) {
			println("usage: Zkeme80ForthCallgraph.java <labelmap.json> [out.csv]");
			return;
		}
		JsonObject data = JsonParser.parseString(
			new String(Files.readAllBytes(Paths.get(args[0])))).getAsJsonObject();

		Map<Long, String> names = new HashMap<>();
		Map<String, Integer> edges = new HashMap<>();

		for (JsonElement el : data.getAsJsonArray("forth_words")) {
			JsonObject w = el.getAsJsonObject();
			names.putIfAbsent(w.get("addr").getAsLong(),
				"forth_" + w.get("name").getAsString());
		}
		for (JsonElement el : data.getAsJsonArray("labels")) {
			JsonObject lab = el.getAsJsonObject();
			String region = lab.has("region") ? lab.get("region").getAsString() : "";
			if (!"ram".equals(region)) {
				names.putIfAbsent(lab.get("addr").getAsLong(),
					lab.get("name").getAsString());
			}
		}

		for (Map.Entry<Long, String> e : new TreeMap<>(names).entrySet()) {
			segs.add(new Entry(e.getKey(), e.getValue()));
		}

		Instruction instr = listing.getInstructionAt(toAddr(0));
		while (instr != null && instr.getAddress().getOffset() < 0x4000) {
			long pc = instr.getAddress().getOffset();
			String caller = owner(pc);
			boolean isCall = instr.getFlowType().isCall();
			for (Address dest : instr.getFlows()) {
				if (isCall) {
					String callee = names.get(dest.getOffset());
					if (callee != null && !callee.equals(caller)) {
						merge(edges, caller, callee, "call");
					}
				}
			}
			if (!isCall) {
				for (Reference ref : instr.getReferencesFrom()) {
					if (ref.getReferenceType().isData()) {
						String callee =
							names.get(ref.getToAddress().getOffset());
						if (callee != null && !callee.equals(caller)) {
							merge(edges, caller, callee, "thread");
						}
					}
				}
			}
			instr = instr.getNext();
		}

		List<String[]> rows = new ArrayList<>();
		for (Map.Entry<String, Integer> e : edges.entrySet()) {
			String[] parts = e.getKey().split("\t");
			rows.add(new String[] { parts[0], parts[1], parts[2],
				e.getValue().toString() });
		}
		rows.sort((a, b) -> {
			int ca = Integer.parseInt(a[3]);
			int cb = Integer.parseInt(b[3]);
			if (ca != cb) {
				return cb - ca;
			}
			return (a[0] + a[1]).compareTo(b[0] + b[1]);
		});

		String outPath = args.length > 1 ? args[1] : "/tmp/zkeme80-callgraph.csv";
		try (PrintWriter pw = new PrintWriter(outPath)) {
			pw.println("caller,callee,kind,count");
			for (String[] r : rows) {
				pw.println(r[0] + "," + r[1] + "," + r[2] + "," + r[3]);
			}
		}
		println("Wrote " + rows.size() + " callgraph edges to " + outPath);
	}

	private void merge(Map<String, Integer> edges, String from, String to,
			String kind) {
		String key = from + "\t" + to + "\t" + kind;
		edges.merge(key, 1, Integer::sum);
	}
}
