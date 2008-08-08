package bigriver.map;

public class Martian extends MapObject {
	public Martian(int x, int y, int dir, int speed) {
		this.x = x;
		this.y = y;
		this.r = 400;
		this.dir = dir;
		this.speed = speed;
	}

	int dir, speed;
	
	@Override
	public String toString() {
		return String.format("Martian: %s, speed: %d, dir: %d", super.toString(), speed, dir);
	}
	
	
}
