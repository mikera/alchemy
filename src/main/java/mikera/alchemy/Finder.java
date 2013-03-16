package mikera.alchemy;

import clojure.lang.IFn;
import mikera.engine.BlockVisitor;

public class Finder extends BlockVisitor<Boolean> {
	
	private IFn callback;

	public Finder(IFn callback) {
		this.callback=callback;
	}
	
	@Override
	public Object visit(int x1, int y1, int z1, int x2, int y2, int z2,
			Boolean value) {
		callback.invoke(x1,y1,z1,value);
		return null;
	}
}
