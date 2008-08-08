package bigriver.planner;

import java.util.List;

import bigriver.command.Command;
import bigriver.map.MapObject;
import bigriver.telemetry.Telemetry;

public class Planner implements Runnable
{
	public int getHomeDir()
	{
		// Get our polar coordinates in 1/10th degrees
		int positionTheta = (int) (Math.atan2(Telemetry.y, Telemetry.x) * 1800.0 / Math.PI);
		// Adjust for quadrant
		if (positionTheta < 0) positionTheta += 3600;
		// Convert our location vector into direction-to-home vector
		positionTheta += 1800;
		while (positionTheta >= 3600) positionTheta -= 3600;
		return positionTheta;
	}
	
	public void update()
	{
//		System.out.println("planner: " + (System.currentTimeMillis() - Telemetry.start));
		if (false && Telemetry.map != null)
		{
			final List<MapObject> obstructions
				= Telemetry.map.findObstructions(Telemetry.x, Telemetry.y, Telemetry.maxSensor, Telemetry.dir, 150);
			if (obstructions.size() > 0)
			{
				System.err.println("Found obstruction: " + obstructions.get(0));
			}
		}
		Command.me.setTargetDir(getHomeDir());
		Command.me.setTargetSpeed(Telemetry.maxSpeed);
	}
	
	public void run()
	{
		try
		{
			while (true)
			{
				synchronized (Telemetry.me)
				{
					Telemetry.me.wait();
				}
				update();
			}
		}
		catch (InterruptedException e)
		{
			System.err.println(e.getMessage());
		}
	}
}
