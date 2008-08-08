package bigriver.map;

import java.util.LinkedList;
import java.util.List;

public class InnerNode extends MapNode {

	MapNode ul, ur, ll, lr;

	public InnerNode(int xmin, int xmax, int ymin, int ymax, int depth) {
		this.xmin = xmin;
		this.xmax = xmax;
		this.ymax = ymax;
		this.ymin = ymin;
		ul = new LeafNode(xmin, (xmax + xmin) / 2, (ymax + ymin) / 2, ymax, depth);
		ur = new LeafNode((xmax + xmin) / 2, xmax, (ymax + ymin) / 2, ymax, depth);
		ll = new LeafNode(xmin, (xmax + xmin) / 2, ymin, (ymax + ymin) / 2, depth);
		lr = new LeafNode((xmax + xmin) / 2, xmax, (ymax + ymin) / 2, ymax, depth);
	}

	@Override
	MapNode put(MapObject obj) {
		if(obj.x < (xmax + xmin) / 2) {
			if (obj.y < (ymax + ymin) / 2) {
				ll = ll.put(obj);
			} else {
				ul = ul.put(obj);
			}
		} else {
			if (obj.y < (ymax + ymin) / 2) {
				lr = lr.put(obj);
			} else {
				ur = ur.put(obj);
			}
		}

		return this;
	}

	@Override
	MapNode update(Martian m) {
		return put(m);
	}

	public String toString(String indent) {
		return String.format("InnerNode: \n%1$sLowerLeft: %2$s\n%1$sLowerRight: $3$s\n%1$sUpperLeft: %4$s\n$1$s UpperRight: %5$s\n",
				indent, ll.toString(indent + "     "), lr.toString(indent + "     "), ul.toString(indent + "     "), ur.toString(indent + "     "));
	}

	@Override
	List<MapObject> findObjects(int xmin, int xmax, int ymin, int ymax) {
		if(xmin > this.xmax || xmax < this.xmin || ymin > this.ymax || ymax < this.ymin) {
			return null;
		}
		List<MapObject> result = new LinkedList<MapObject>();
		List<MapObject> temp;
		temp = ll.findObjects(xmin, xmax, ymin, ymax);;
		if(temp == null) {
			result.addAll(temp);
		} 
		temp = lr.findObjects(xmin, xmax, ymin, ymax);
		if(temp == null) {
			result.addAll(temp);
		}
		temp = ur.findObjects(xmin, xmax, ymin, ymax);
		if(temp == null) {
			result.addAll(temp);
		}
		temp = ul.findObjects(xmin, xmax, ymin, ymax);
		if(temp == null) {
			result.addAll(temp);
		}
		return result;
	}

}
