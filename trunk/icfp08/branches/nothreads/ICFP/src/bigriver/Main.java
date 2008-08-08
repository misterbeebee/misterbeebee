package bigriver;

import java.net.Socket;

import bigriver.telemetry.Telemetry;

public class Main
{
	public static void main(String[] args) throws Exception
	{
		new Telemetry(new Socket(args[0], Integer.parseInt(args[1]))).run();
	}
}
