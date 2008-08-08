package bigriver.telemetry;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

import bigriver.command.Command;
import bigriver.map.Boulder;
import bigriver.map.Crater;
import bigriver.map.MapObject;
import bigriver.map.MarsMap;
import bigriver.map.Martian;

public class Telemetry
{
	private MarsMap map;

	public int dx, dy;
	public int timeLimit;
	public int minSensor;
	public int maxSensor;
	public int maxSpeed;
	public int maxTurn;
	public int maxHardTurn;
	
	public int time;
	public int timeStep;
	public char throttle, turn;
	public int x, y, speed;
	public int dir, vDir;

    private InputStream in_;
	private Command command_;
	private int prevDir_;

    public Telemetry(Socket socket) throws Exception
    {
        in_ = new BufferedInputStream(socket.getInputStream());
        command_ = new Command(socket.getOutputStream());
//      Planner planner = new Planner();
    }

    public void run()
    {
        try
        {
            for (int ch = in_.read(); ch >= 0; ch = in_.read())
            {
                switch (ch)
                {
                case 'I':
                    System.out.println("Got init");
                    dx = readMilli();
                    dy = readMilli();
                    timeLimit = readNumber();
                    minSensor = readMilli();
                    maxSensor = readMilli();
                    maxSpeed = readMilli();
                    maxTurn = readDeci();
                    maxHardTurn = readDeci();
                    map = new MarsMap(dx, dy);
                    System.out.println("dx: " + dx + " dy: " + dy + " timeLimit: " + timeLimit
                		+ " minSensor: " + minSensor + " maxSensor: " + maxSensor + " maxSpeed: " + maxSpeed
                		+ " maxTurn: " + maxTurn + " maxHardTurn: " + maxHardTurn);
                    break;
                case 'T':
                    time = readNumber();
                    if (timeStep == 0) timeStep = time;
                    throttle = (char)in_.read();
                    turn = (char)in_.read();
                    x = readMilli();
                    y = readMilli();
                    prevDir_ = dir;
                    dir = readDeci();
                    vDir = dir - prevDir_;
                    speed = readMilli();
                    List<MapObject> objects = new ArrayList<MapObject>(); 
                    loop: for (;;)
                    {
                    	do
                		{
                            ch = in_.read();
                		}
                    	while (ch == ' ');
	                	switch (ch)
	                	{
	                	case 'b':
	                	    objects.add(new Boulder(readMilli(), readMilli(), readMilli()));
	                	    break;
				        case 'c':
				            objects.add(new Crater(readMilli(), readMilli(), readMilli()));
				            break;
				        case 'h':
				            readMilli(); readMilli(); readMilli();
				            break;
				        case 'm':
				            objects.add(new Martian(readMilli(), readMilli(), readDeci(), readMilli()));
				            break;
				        default:
				            break loop;
				        }
				    }
                    System.out.println("t: " + time + " throttle: " + throttle + " turn: " + turn + " x: " + x + " y: " + y + " dir: " + dir + " speed: " + speed);
                    map.update(x, y, dir, time, objects);
                	command_.update(this);
                    break;
                case 'B':
                    System.out.println("Got bounce");
                    break;
                case 'C':
                    System.out.println("Got cratered");
                    break;
                case 'K':
                    System.out.println("Got killed");
                    break;
                case 'S':
                    System.out.println("Got success");
                    break;
                case 'E':
                    System.out.println("Got end of run");
                    map.clearMartians();
                    break;
                }
                while (ch >= 0 && ch != ';') ch = in_.read();
            }
        } catch (Exception e) {
            System.out.println("Got Exception: " + e);
            System.exit(0);
        }
    }

    private int readNumber() throws IOException {
        return readMilli() / 1000;
    }
    
    private int readMilli() throws IOException {
        int ch = in_.read();
        while (ch == ' ') ch = in_.read();
        int sign = ch == '-' ? -1 : 1;
        if (sign < 0) ch = in_.read();
        int whole = 0;
        while (Character.isDigit(ch))
        {
            whole = whole * 10 + ch - '0';
            ch = in_.read();
        }
        whole *= 1000;
        if (ch == '.') {
            ch = in_.read();
            int digits = 0;
            int frac = 0;
            while (Character.isDigit(ch))
            {
                digits++;
                frac = frac * 10 + ch - '0';
                ch = in_.read();
            }
            while (digits < 3) { digits++; frac = frac * 10; }
            whole += frac;
        }
        return sign * whole;
    }

    private int readDeci() throws IOException {
        int ch = in_.read();
        while (ch == ' ') ch = in_.read();
        int sign = ch == '-' ? -1 : 1;
        if (sign < 0) ch = in_.read();
        int whole = 0;
        while (Character.isDigit(ch))
        {
            whole = whole * 10 + ch - '0';
            ch = in_.read();
        }
        whole *= 10;
        if (ch == '.') {
            ch = in_.read();
            int digits = 0;
            int frac = 0;
            while (Character.isDigit(ch))
            {
                digits++;
                frac = frac * 10 + ch - '0';
                ch = in_.read();
            }
            while (digits < 1) { digits++; frac = frac * 10; }
            whole += frac;
        }
        return sign * whole;
    }
}
