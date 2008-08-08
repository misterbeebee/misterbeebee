package bigriver.map;

public class Crater extends MapObject {
	public Crater(int x, int y, int r) {
		this.x = x;
		this.y = y;
		this.r = r;
	}

	@Override
	public String toString() {
		return String.format("Crater: %s", super.toString());
	}
	
}
