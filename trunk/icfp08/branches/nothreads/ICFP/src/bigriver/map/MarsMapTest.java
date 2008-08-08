package bigriver.map;

import junit.framework.TestCase;

public class MarsMapTest extends TestCase {

	MarsMap theMap = new MarsMap(10000, 20000);
	
	public void testFindObstructions() {
		assertNotNull(theMap.findObstructions(0, 0, 1000, 450, 15));
	}

}
