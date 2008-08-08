package bigriver.map;

public class Boulder extends MapObject {
	public Boulder(int x, int y, int r) {
		this.x = x;
		this.y = y;
		this.r = r;
	}

	@Override
	public String toString() {
		return String.format("Boulder: $s", super.toString());
	}
	
}
