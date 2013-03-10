package mikera.alchemy;

import mikera.cljutils.Clojure;

public class GameApp {

	public static void main(String[] args) {
		// just launch the Clojure code
		Clojure.require("mikera.alchemy.main");
		Clojure.eval("(mikera.alchemy.main/main)");	
	}

}
