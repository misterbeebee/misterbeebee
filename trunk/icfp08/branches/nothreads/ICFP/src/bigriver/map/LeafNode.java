package bigriver.map;

import java.util.LinkedList;
import java.util.List;

public class LeafNode extends MapNode {

	List<MapObject> objs = new LinkedList<MapObject>();
	int lastTime;

	public LeafNode(int xmin, int xmax, int ymin, int ymax, int depth) {
		this.xmin = xmin;
		this.xmax = xmax;
		this.ymax = ymax;
		this.ymin = ymin;
	}

	@Override
	MapNode put(MapObject obj) {
		if(objs.size() > MAX_OBJECTS_PER_LEAFNODE && depth < MAX_TREE_DEPTH) {
			MapNode result = new InnerNode(xmin, xmax, ymin, ymax, depth);
			for(MapObject o: objs) {
				result.put(o);
			}
			return result;
		}else {
			for(MapObject o: objs) {
				if(o.x == obj.x && o.y == obj.y) {
					return this;
				}
			}
			objs.add(obj);
			return this;
		}
	}

	@Override
	MapNode update(Martian m) {
		return put(m);
	}

	@Override
	String toString(String indent) {
		String result = indent + "LeafNode: Objects:\n";
		indent += "     ";
		for(MapObject obj: objs) {
			result += indent +obj.toString() + "\n";
		}
		return result;
	}

	@Override
	List<MapObject> findObjects(int xmin, int xmax, int ymin, int ymax) {
		if(xmin > this.xmax || xmax < this.xmin || ymin > this.ymax || ymax < this.ymin) {
			return null;
		}
		List<MapObject> result = new LinkedList<MapObject>();
		for(MapObject obj: objs) {
			if(obj.x > xmin && obj.x < xmax && obj.y > ymin && obj.y < ymax) {
				result.add(obj);
			}
		}
		return result;
	}



}
