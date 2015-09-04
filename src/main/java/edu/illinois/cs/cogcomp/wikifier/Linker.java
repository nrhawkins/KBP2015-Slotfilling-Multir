package edu.illinois.cs.cogcomp.wikifier;


public class Linker {
  public static Linker instance = null;
  

  public static Linker instance() {
    if (instance == null) {
      instance = new Linker();
    }
    return instance;
  }

  public static void main(String[] args) {
    //Annotation ann = null;
    //FileInputStream fis = null;
    //ObjectInputStream in = null;
    System.out.println("Hello, World!");
  }

}
