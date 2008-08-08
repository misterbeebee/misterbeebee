package bigriver.command;

import java.io.OutputStream;

import bigriver.telemetry.Telemetry;

public class Command
{
	private OutputStream out_;
	
	private volatile int targetDir_;
	private volatile int targetSpeed_;
	private int requestedTurn_;
	
	private static final int NETWORK_SIZE = 30;
	private double[][] activations_ = new double[3][NETWORK_SIZE];
	private double[][] weights_		= new double[2][NETWORK_SIZE];
	
	public Command(OutputStream out)
	{
		this.out_ = out;
		requestedTurn_ = 0;
		// Need to initialize the network statically - "pretrained"
	}
	
	public void setTargetDir(int targetDir)
	{
		targetDir_ = targetDir;
	}
	
	public void setTargetSpeed(int targetSpeed)
	{
		targetSpeed_ = targetSpeed;
	}
	
	private void turn(int requested, String msg) throws Exception
	{
		if (requestedTurn_ != requested)
		{
			while (requested > requestedTurn_)
			{
				out_.write('r');
				out_.write(';');
				++requestedTurn_;
			}
			while (requested < requestedTurn_)
			{
				out_.write('l');
				out_.write(';');
				--requestedTurn_;
			}
			System.out.println(msg);
		}
	}
	
	public void update(Telemetry telem) throws Exception
	{
		// Get our polar coordinates in 1/10th degrees
		int positionTheta = (int) (Math.atan2(telem.y, telem.x) * 1800.0 / Math.PI);
		// Adjust for quadrant
		if (positionTheta < 0) positionTheta += 3600;
		// Convert our location vector into direction-to-home vector
		positionTheta += 1800;
		while (positionTheta >= 3600) positionTheta -= 3600;
		targetDir_ = positionTheta;
		
		int deltaTheta = targetDir_ - telem.dir;
		// Normalize angle to make a half turn or less
		if (deltaTheta > 1800) deltaTheta -= 3600;
		if (deltaTheta < -1800) deltaTheta += 3600;
		System.out.println("dTheta: " + deltaTheta + " vDir: " + telem.vDir);
		
		int softThreshold = telem.maxTurn * telem.timeStep / 1000;
		int hardThreshold = telem.maxHardTurn * telem.timeStep / 1000;
		if (deltaTheta < 0)					// Need right
	    {
			if (deltaTheta >= -softThreshold)
			{
				turn(0, "Centering: " + deltaTheta);
			}
			else if (deltaTheta >= -hardThreshold * 5)		// Near center
			{
				if (telem.vDir < -hardThreshold)	// Need hard right
				{
					turn(+2, "Hard right: " + deltaTheta);
				}
				else if (telem.vDir < softThreshold)	// Need soft right
				{
					turn(+1, "Soft right: " + deltaTheta);
				}
				else if (telem.vDir >= hardThreshold)	// Hard slow down
				{
					turn(-2, "Centering hard left: " + deltaTheta);
				}
				else if (telem.vDir >= softThreshold)	// Slow down
				{
					turn(-1, "Centering left: " + deltaTheta);
				}
			}
			else
			{
				turn(+2, "Hard right: " + deltaTheta);
			}
	    }
		else if (deltaTheta > 0)                // Need left
		{
			if (deltaTheta <= softThreshold)
			{
				turn(0, "Centering: " + deltaTheta);
			}
			else if (deltaTheta <= hardThreshold * 5)		// Near center
			{
				if (telem.vDir > hardThreshold)		// Need hard left
				{
					turn(-2, "Hard left: " + deltaTheta);
				}
				else if (telem.vDir > softThreshold)	// Need soft left
				{
					turn(-1, "Soft left: " + deltaTheta);
				}
				else if (telem.vDir <= -hardThreshold)	// Hard slow down
				{
					turn(+2, "Centering hard right: " + deltaTheta);
				}
				else if (telem.vDir <= -softThreshold)	// Slow down
				{
					turn(+1, "Centering right: " + deltaTheta);
				}
			}
			else
			{
				turn(-2, "Hard left: " + deltaTheta);
			}
		}
		targetSpeed_ = telem.maxSpeed;
		int deltaSpeed = targetSpeed_ - telem.speed;
		if (deltaSpeed > 0)
		{
	        if (telem.throttle != 'a')
	        {
                out_.write('a');
                out_.write(';');
	        }
		}
		else if (deltaSpeed < 0)
		{
	        if (telem.throttle != 'b')
	        {
	            out_.write('b');
	            out_.write(';');
            }
        }
	}
}
