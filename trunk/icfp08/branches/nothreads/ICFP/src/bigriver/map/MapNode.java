package bigriver.map;

import java.util.List;

public abstract class MapNode {
	
	int xmin, xmax, ymax, ymin;
	int depth = 0;
	
	protected static final int MAX_OBJECTS_PER_LEAFNODE = 16;
	protected static final int MAX_TREE_DEPTH = 1024;
	
	abstract MapNode put(MapObject obj);
	
	/**assumes the quadtree THIS is a part of conatins only
	 * martians
	 * @param m martian the rover saw
	 * @return this or MapNode that should replace this
	 */
	abstract MapNode update(Martian m);
	
	abstract String toString(String indent);
	
	abstract List<MapObject> findObjects(int xmin, int xmax, int ymin, int ymax);
	
	
	public String toString() {
		return toString("");
	}
	
}
