package mikera.alchemy;

import mikera.engine.BitGrid;
import mikera.engine.BlockVisitor;

public class BitGridExtender extends BlockVisitor<Boolean> {
	public BitGrid bg;
	public BitGrid result;
	
	public BitGridExtender(BitGrid bg) {
		this.bg=bg;
		this.result=bg.clone();
	}
	
	@Override
	public Object visit(int x1, int y1, int z1, int x2, int y2, int z2,
			Boolean value) {
		if (!value) return null;
		for (int dx=-1; dx<=1; dx++) {
			for (int dy=-1; dy<=1; dy++) {
				result.set(x1+dx,y1+dy,z1,true);
			}
		}
		return null;
	}


}
