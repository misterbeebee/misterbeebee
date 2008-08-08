package bigriver.map;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

public class MarsMap {

	int xmin, xmax, ymin, ymax;
	MapNode rock = new InnerNode(xmin, xmax, ymin, ymax, 0);
	MapNode martians = new InnerNode(xmin, xmax, ymin, ymax, 0);
	private int maxRadius = 500;

	public MarsMap(int dx, int dy) {
		xmin = -1 * (xmax = dx/2);
		ymin = -1 * (ymax = dy/2);
	}

	public void update(int x, int y, int dir, int time, List<MapObject> objs) {
		MapNode result = new InnerNode(xmin, xmax, ymin, ymax, 0);
		for(MapObject o: objs) {
			if(Martian.class.isInstance(o)) {
				result.update((Martian) o);
			} else {
				rock.put(o);
			}
		}
		martians = result;
	}

	public void clearMartians() {
		martians = new InnerNode(xmin, xmax, ymin, ymax, 0);
	}

	public List<MapObject> findObstructions(int x, int y, int dist, int dir, int coneWidth) {
		List<MapObject> results = new LinkedList<MapObject>();
		List<MapObject> temp;
		temp = martians.findObjects(x - (dist + maxRadius ), x + dist + maxRadius, y - (dist + maxRadius), y + dist + maxRadius);
		if(temp == null) {
			results.addAll(temp);
		}
		temp = rock.findObjects(x - (dist + maxRadius ), x + dist + maxRadius, y - (dist + maxRadius), y + dist + maxRadius);
		if(temp == null) {
			results.addAll(temp);
		}
		ListIterator<MapObject> iter = results.listIterator();
		while(iter.hasNext()) {
			MapObject obj = iter.next();
			if(Math.abs(dir - (int) (Math.atan2(obj.y - y, obj.x - x) * 1800.0 / Math.PI)) > coneWidth) {
				int x2 = (int) Math.cos(dir /1800.0 * Math.PI) * dist;
				int y2 = (int) Math.sin(dir /1800.0 * Math.PI) * dist;
				int u = ((obj.x - x) * (x2 - x) + (obj.y - y) * (y2 - y)) / (dist * dist);
				int xi = x + u * (x2 - x);
				int yi = y + u * (y2 - y);
				int distance = (int) Math.sqrt((xi - obj.x) * (xi - obj.x) + (yi - obj.y) * (yi - obj.y));
				if(distance > obj.r + 500) {
					iter.remove();
				}
			}
		}
		return results;
	}

	@Override
	public String toString() {
		return rock.toString() + martians.toString();
	}

}
