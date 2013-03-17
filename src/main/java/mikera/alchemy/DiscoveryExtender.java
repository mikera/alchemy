package mikera.alchemy;

import clojure.lang.IFn;
import mikera.engine.BitGrid;
import mikera.engine.BlockVisitor;
import mikera.engine.PersistentTreeGrid;
import mikera.util.Tools;

public class DiscoveryExtender extends BlockVisitor<Boolean> {
	public BitGrid bg;
	
	public IFn func;
	public PersistentTreeGrid<Object> grid;
	
	public DiscoveryExtender(BitGrid bg, PersistentTreeGrid<Object> dg, IFn func) {
		this.bg=bg;
		this.grid=dg;
		this.func=func;
	}
	
	@Override
	public Object visit(int x1, int y1, int z1, int x2, int y2, int z2,
			Boolean value) {
		if (!value) return null;
		Object v=grid.get(x1, y1, z1);
		Object sv=func.invoke(x1, y1, z1);
		if (!Tools.equalsWithNulls(v, sv)) {
			grid=grid.set(x1, y1, z1, sv);
		}
		return null;
	}
}
