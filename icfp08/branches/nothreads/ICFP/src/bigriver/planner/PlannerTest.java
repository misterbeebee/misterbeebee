package bigriver.planner;

import junit.framework.TestCase;
import bigriver.telemetry.Telemetry;

public class PlannerTest extends TestCase
{
	public void testGetHomeDir1() {
		Telemetry.x = 5;
		Telemetry.y = 5;
		Planner planner = new Planner();
		assertEquals(2250, planner.getHomeDir());
	}
	public void testGetHomeDir2() {
		Telemetry.x = -5;
		Telemetry.y = 5;
		Planner planner = new Planner();
		assertEquals(3150, planner.getHomeDir());
	}
	public void testGetHomeDir3() {
		Telemetry.x = -5;
		Telemetry.y = -5;
		Planner planner = new Planner();
		assertEquals(450, planner.getHomeDir());
	}
	public void testGetHomeDir4() {
		Telemetry.x = 5;
		Telemetry.y = -5;
		Planner planner = new Planner();
		assertEquals(1350, planner.getHomeDir());
	}
}
