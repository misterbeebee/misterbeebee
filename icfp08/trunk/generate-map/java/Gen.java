import java.util.*;
import java.io.*;
import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.event.*;
import java.awt.image.*;

public class Gen extends JFrame {
  private static final int SIZE = 800;
  private static final String BLANK_DOC = "{\"timeLimit\":30000,\"boulders\":[],\"craters\":[],\"martianParams\":{\"hardTurn\":60,\"turn\":20,\"rotAccel\":120,\"maxSpeed\":20,\"accel\":3,\"rearView\":30,\"frontView\":60,\"brake\":2},\"runs\":[{\"enemies\":[],\"vehicle\":{\"dir\":0,\"y\":-50,\"x\":0}}],\"vehicleParams\":{\"hardTurn\":60,\"turn\":20,\"rotAccel\":120,\"maxSpeed\":20,\"accel\":3,\"rearView\":30,\"frontView\":60,\"brake\":2},\"size\":200}";

  private String filePath;
  private Field doc;
  private double mapSize;

  private BufferedImage image;
  private Panel mainPanel;

  private boolean mouseDown;
  private int addState; // 0=nothing, 1=boulder, 2=crater, 3=vehicle, 4=enemy
  private int runIndex;

  public Gen(String filePath) {
    this.filePath = filePath;
    addState = 0;
    mouseDown = false;
    runIndex = 0;
    mapSize = 100;
    doc = new Field("", BLANK_DOC);
    if(filePath.length() > 0) openFile();
  }

  public void initalizeComponents() {
    image = new BufferedImage(SIZE, SIZE, BufferedImage.TYPE_INT_RGB);
    mainPanel = new Panel() { public void paint(Graphics g) { g.drawImage(image, 0, 0, Color.WHITE, this); } };
    Panel buttonPanel = new Panel();
    Panel toolbarPanel = new Panel();
    
    ArrayList<Button> buttonPanelButtons = new ArrayList<Button>();
    ArrayList<Button> toolbarPanelButtons = new ArrayList<Button>();
    
    buttonPanelButtons.add(new MButton("Open") { public void actionPerformed(ActionEvent e) {
      JFileChooser fc = new JFileChooser(); fc.setDialogType(JFileChooser.OPEN_DIALOG); fc.setDialogTitle("Open");
      if(fc.showOpenDialog(Gen.this) == JFileChooser.APPROVE_OPTION) { filePath = fc.getSelectedFile().getPath(); openFile(); } } } );

    buttonPanelButtons.add(new MButton("Save") { public void actionPerformed(ActionEvent e) { saveFile(); } } );
    buttonPanelButtons.add(new MButton("Save As") { public void actionPerformed(ActionEvent e) {
      JFileChooser fc = new JFileChooser(); fc.setDialogType(JFileChooser.SAVE_DIALOG); fc.setDialogTitle("Save As");
      if(fc.showOpenDialog(Gen.this) == JFileChooser.APPROVE_OPTION) { filePath = fc.getSelectedFile().getPath(); saveFile(); } } } );
    
    
    toolbarPanelButtons.add(new MButton("None") { public void actionPerformed(ActionEvent e) { addState = 0; } } );
    toolbarPanelButtons.add(new MButton("Boulder") { public void actionPerformed(ActionEvent e) { addState = 1; } } );
    toolbarPanelButtons.add(new MButton("Crater") { public void actionPerformed(ActionEvent e) { addState = 2; } } );
    toolbarPanelButtons.add(new MButton("Vehicle") { public void actionPerformed(ActionEvent e) { addState = 3; } } );
    toolbarPanelButtons.add(new MButton("Enemy") { public void actionPerformed(ActionEvent e) { addState = 4; } } );
    toolbarPanelButtons.add(new MButton("Next Run") { public void actionPerformed(ActionEvent e) { runIndex++; runIndex %= doc.map.get("runs").list.size(); paintTerrain(); } } );
    toolbarPanelButtons.add(new MButton("Prev Run") { public void actionPerformed(ActionEvent e) { runIndex--; if(runIndex == -1) runIndex = doc.map.get("runs").list.size() - 1; paintTerrain(); } } );
    toolbarPanelButtons.add(new MButton("Add Run") { public void actionPerformed(ActionEvent e) { doc.map.get("runs").list.add(new Field("", "{\"enemies\":[],\"vehicle\":{\"dir\":0,\"y\":-100,\"x\":0}}")); runIndex = doc.map.get("runs").list.size() - 1; paintTerrain(); } } );
    
    mainPanel.setMinimumSize(new Dimension(SIZE, SIZE));
    buttonPanel.setLayout(new GridLayout(1, buttonPanelButtons.size()));
    for(Button button : buttonPanelButtons) {
      buttonPanel.add(button);
    }
    toolbarPanel.setLayout(new GridLayout(toolbarPanelButtons.size(), 1));
    for(Button button : toolbarPanelButtons) {
      toolbarPanel.add(button);
    }
    
    Panel panel1 = new Panel();
    Panel panel2 = new Panel();
    panel1.setLayout(new BorderLayout());
    panel2.setLayout(new BorderLayout());
    panel1.add(toolbarPanel, BorderLayout.EAST);
    panel1.add(mainPanel, BorderLayout.CENTER);
    panel2.add(buttonPanel, BorderLayout.SOUTH);
    panel2.add(panel1, BorderLayout.CENTER);
    getContentPane().add(panel2);
    
    setPreferredSize(new Dimension(SIZE + 100, SIZE + 100));
    validate();
    pack();

    MouseInputListener listener = new MouseInputListener() {
      Field f;
      double fx, fy;
      public void update(MouseEvent e) { if(f == null) return; 
          if(addState == 1 || addState == 2) f.map.get("r").value = "" + (2.0 * Math.sqrt((e.getX() - fx) * (e.getX() - fx) + ((SIZE - e.getY()) - fy) * ((SIZE - e.getY()) - fy)) / SIZE * mapSize);
          if(addState == 3 || addState == 4) f.map.get("dir").value = "" + (180 / Math.PI * Math.atan2((SIZE - e.getY()) - fy, e.getX() - fx));
          paintTerrain(); }
      public void 	mouseClicked(MouseEvent e) {}
      public void 	mouseEntered(MouseEvent e) {}
      public void 	mouseExited(MouseEvent e) {}
      public void 	mousePressed(MouseEvent e) { mouseDown = true; fx = e.getX(); fy = (SIZE - e.getY()); if(addState == 0) return;
          if(addState == 1 || addState == 2) f = new Field("", "{\"x\":" + (2.0 * e.getX() / SIZE * mapSize - mapSize) + ",\"y\":" + (2.0 * (SIZE - e.getY()) / SIZE * mapSize - mapSize) + ",\"r\":0}");
          if(addState == 3) { f = new Field("vehicle", "{\"x\":" + (2.0 * e.getX() / SIZE * mapSize - mapSize) + ",\"y\":" + (2.0 * (SIZE - e.getY()) / SIZE * mapSize - mapSize) + ",\"dir\":0}");
                                  doc.map.get("runs").list.get(runIndex).map.put("vehicle", f); }
          if(addState == 4) { f = new Field("", "{\"x\":" + (2.0 * e.getX() / SIZE * mapSize - mapSize) + ",\"y\":" + (2.0 * (SIZE - e.getY()) / SIZE * mapSize - mapSize) + ",\"dir\":0,\"speed\":0.25,\"ai\":\"seek\",\"view\":60}");
                                  doc.map.get("runs").list.get(runIndex).map.get("enemies").list.add(f); }
          if(addState == 1) doc.map.get("boulders").list.add(f); else if(addState == 2) doc.map.get("craters").list.add(f); update(e);  } 
      public void 	mouseReleased(MouseEvent e) { mouseDown = false; update(e); f = null; }
      public void 	mouseDragged(MouseEvent e) { update(e); }
      public void 	mouseMoved(MouseEvent e) {}
    };
    mainPanel.addMouseListener(listener);
    mainPanel.addMouseMotionListener(listener);
    
    paintTerrain();
  }

  void openFile() {
    FileReader reader;
    try {
      reader = new FileReader(filePath);
    } catch(FileNotFoundException e) {
      System.out.println("File does not exist - " + e);
      return;
    }
    
    try {
      StringBuffer dat = new StringBuffer();
      int ch;
      while((ch = reader.read()) != -1) {
        dat.append((char)ch);
      }
      doc = new Field("", dat.toString());
    } catch(IOException e) {
      System.out.println("Read exception - " + e);
    }
    
    runIndex = 0;
    mapSize = 1;
    try {
      mapSize = Double.parseDouble(doc.map.get("size").value) / 2.0;
    } catch(NullPointerException e) {
      System.out.println("Unexpected document format");
    } catch(NumberFormatException e) {
      System.out.println("Unexpected document format");
    }
    
    if(image != null) paintTerrain();
  }
  
  void saveFile() {
    try {
      FileWriter writer = new FileWriter(filePath);
      writer.write(doc.toString());
      writer.flush();
      writer.close();
    } catch(FileNotFoundException e) {
      System.out.println("File does not exist - " + e);
    } catch(IOException e) {
      System.out.println("Read exception - " + e);
    }
  }

  public void paintTerrain() {
    Graphics g = image.getGraphics();
    g.setColor(Color.WHITE);
    g.fillRect(0, 0, SIZE, SIZE);
    g.setColor(Color.GREEN);
    g.fillOval(SIZE / 2 - 5, SIZE / 2 - 5, 10, 10);
    try {
      g.setColor(Color.BLACK);
      for(Field boulder : doc.map.get("boulders").list) {
        double x = Double.parseDouble(boulder.map.get("x").value); x += mapSize; x /= 2 * mapSize / SIZE;
        double y = Double.parseDouble(boulder.map.get("y").value); y += mapSize; y /= 2 * mapSize / SIZE; y = SIZE - y;
        double r = Double.parseDouble(boulder.map.get("r").value); r /= 2 * mapSize / SIZE;
        g.fillOval((int)(x - r), (int)(y - r), (int)(2 * r), (int)(2 * r));
      }
      g.setColor(Color.YELLOW);
      for(Field craters : doc.map.get("craters").list) {
        double x = Double.parseDouble(craters.map.get("x").value); x += mapSize; x /= 2 * mapSize / SIZE;
        double y = Double.parseDouble(craters.map.get("y").value); y += mapSize; y /= 2 * mapSize / SIZE; y = SIZE - y;
        double r = Double.parseDouble(craters.map.get("r").value); r /= 2 * mapSize / SIZE;
        g.fillOval((int)(x - r), (int)(y - r), (int)(2 * r), (int)(2 * r));
      }
      
      {
      g.setColor(Color.BLUE);
      Field veh = doc.map.get("runs").list.get(runIndex).map.get("vehicle");
      double x = Double.parseDouble(veh.map.get("x").value); x += mapSize; x /= 2 * mapSize / SIZE;
      double y = Double.parseDouble(veh.map.get("y").value); y += mapSize; y /= 2 * mapSize / SIZE; y = SIZE - y;
      double dir = Double.parseDouble(veh.map.get("dir").value);
      g.fillOval((int)(x - 4), (int)(y - 4), 8, 8);
      g.setColor(Color.BLACK);
      g.drawLine((int)x, (int)y, (int)(x + 4 * Math.cos(dir / 180 * Math.PI)), (int)(y + 4 * -Math.sin(dir / 180 * Math.PI)));
      }
      
      for(Field enemy : doc.map.get("runs").list.get(runIndex).map.get("enemies").list) {
        g.setColor(Color.RED);
        double x = Double.parseDouble(enemy.map.get("x").value); x += mapSize; x /= 2 * mapSize / SIZE;
        double y = Double.parseDouble(enemy.map.get("y").value); y += mapSize; y /= 2 * mapSize / SIZE; y = SIZE - y;
        double dir = Double.parseDouble(enemy.map.get("dir").value);
        g.fillOval((int)(x - 4), (int)(y - 4), 8, 8);
        g.setColor(Color.BLACK);
        g.drawLine((int)x, (int)y, (int)(x + 4 * Math.cos(dir / 180 * Math.PI)), (int)(y + 4 * -Math.sin(dir / 180 * Math.PI)));
      }
    } catch(NullPointerException e) {
      System.out.println("Unexpected document format");
    } catch(NumberFormatException e) {
      System.out.println("Unexpected document format");
    }
    mainPanel.repaint();
  }

  public static void main(String [] args) {
    Gen gen = new Gen(args.length > 0 ? args[0] : "");
    gen.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    gen.initalizeComponents();
    gen.setVisible(true);
  }
  
  private static class Field {
    public int type; //1==name/value 2==map 3==list
    public String name;
    public String value;
    public Map<String, Field> map;
    public ArrayList<Field> list;
    
    public Field(String namep, String valuep) {
      this.value = valuep.trim();
      this.name = namep.trim();
      type = 1;
      if(value.length() > 0)
        if(value.charAt(0) == '{')
          type = 2;
        else if(value.charAt(0) == '[')
          type = 3;
        
      map = null;
      list = null;
      if(type == 2) map = new HashMap<String, Field>();
      if(type == 3) list = new ArrayList<Field>();
      if(type == 2 || type == 3) {
        int state = type == 2 ? 0 : 1;
        int paren = 0;
        boolean inQuote = false;
        StringBuffer nameBuf = new StringBuffer();
        StringBuffer valueBuf = new StringBuffer();
        for(int i = 1; i < value.length(); i++) {
          char ch = value.charAt(i);
          if(state == 0) {
            if(ch == '\"') inQuote = !inQuote;
            if(!inQuote && ch == ':')
              state = 1;
            else if(inQuote && ch != '\"')
              nameBuf.append(ch);
          } else if(state == 1) {
            if(ch == '{' || ch == '[')
              paren++;
            else if(ch == '}' || ch == ']')
              paren--;
            if(paren == 0 && ch == ',' || paren == -1) {
              Field child = new Field(nameBuf.toString(), valueBuf.toString());
              if(child.name.length() > 0 || child.value.length() > 0) {
                if(type == 2) map.put(child.name, child);
                if(type == 3) list.add(child);
              }
              nameBuf = new StringBuffer();
              valueBuf = new StringBuffer();
              state = type == 2 ? 0 : 1;
            } else {
              valueBuf.append(ch);
            }
          }
        }
      }
    }
    
    public String toString() {
      StringBuffer ret = new StringBuffer();
      outputTo(ret, "");
      return ret.toString();
    }
    
    public void outputTo(StringBuffer buf, String prefix) {
      if(type == 1) {
        buf.append(prefix).append("\"").append(name).append("\" : ").append(value);
      } else if(type == 2 || type == 3) {
        buf.append(prefix);
        if(name.length() > 0) buf.append("\"").append(name).append("\" : ");
        buf.append(type == 2 ? "{" : "[").append("\n");
        int cnt = 0;
        if(type == 2) for(String fieldName : map.keySet()) {
          cnt++;
          Field field = map.get(fieldName);
          field.outputTo(buf, prefix + " ");
          if(cnt < map.size()) buf.append(",");
          buf.append("\n");
        }
        if(type == 3) for(Field field : list) {
          cnt++;
          field.outputTo(buf, prefix + " ");
          if(cnt < list.size()) buf.append(",");
          buf.append("\n");
        }
        buf.append(prefix).append(type == 2 ? "}" : ']');
      }
    }
  }
  
  private abstract static class MButton extends Button implements ActionListener
  {
    public MButton(String name) {
      super(name);
      addActionListener(this);
    }
  }
}

