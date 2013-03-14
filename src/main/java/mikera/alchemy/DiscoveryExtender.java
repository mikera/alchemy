package mikera.alchemy;

import mikera.engine.BitGrid;
import mikera.engine.BlockVisitor;
import mikera.engine.PersistentTreeGrid;

public class DiscoveryExtender extends BlockVisitor<Boolean> {
	public BitGrid bg;
	
	public PersistentTreeGrid<Object> source;
	public PersistentTreeGrid<Object> grid;
	
	public DiscoveryExtender(BitGrid bg, PersistentTreeGrid<Object> dg, PersistentTreeGrid<Object> source) {
		this.bg=bg;
		this.grid=dg;
		this.source=source;
	}
	
	@Override
	public Object visit(int x1, int y1, int z1, int x2, int y2, int z2,
			Boolean value) {
		if (!value) return null;
		if (grid.get(x1, y1, z1)== null) {
			Object sv=source.get(x1, y1, z1);
			if (sv!=null) {
				grid=grid.set(x1, y1, z1, sv);
			}
		}
		return null;
	}
}
