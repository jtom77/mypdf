package mypdf.font;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

/**
 * Created by ThomasE on 16.02.2016.
 */
public class HexToUnicode {

    Map<Integer, String> hexToUni;

    public void readUnicode() {
        hexToUni = new TreeMap<>();
        StringBuffer buf = null;
        String hex = null;
        String unicode = null;
        try {
            FileInputStream in = new FileInputStream(new File("cmap2.txt"));
            int code;
            while ((code = in.read()) != -1) {
                char c = (char) code;
                switch (c) {
                    case '<':
                        buf = new StringBuffer();
                        break;
                    case '>':
                        if (hex == null) {
                            hex = buf.toString();
                        } else {
                            unicode = unicodeIntToString(buf.toString());
                            hexToUni.put(Integer.parseInt(hex, 16), unicode);
                            hex = null;
                            unicode = null;
                        }
                        buf = null;
                        break;
                    default:
                        if (buf != null) {
                            buf.append(c);
                        }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    public void decodeString() {
        try {
            FileInputStream in = new FileInputStream(new File("text2.txt"));
            StringBuilder buf = new StringBuilder();
            String hex = null;
            int code;
            // <1f1e1d1c1b>
            while ((code = in.read()) != -1) {
                char c = (char) code;
                switch (c) {
                    case '<':
                        hex = "";
                        break;
                    case '>':
                        hex = null;
                        break;
                    default:
                        if (hex != null) {
                            switch (hex.length()) {
                                case 0:
                                    hex = hex + c;
                                    break;
                                case 1:
                                    hex = hex + c;
                                    try {
                                        Integer key = Integer.parseInt(hex, 16);
                                        String unicode = hexToUni.get(key);
                                        hex = "";
                                        if (unicode != null) {
                                            buf.append(unicode);
                                        }
                                    } catch (NumberFormatException e) {
                                        hex = null;

                                    }
                            }
                        }
                }



            }

            System.out.println(buf);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    private String unicodeIntToString(String intVal) {
        StringBuilder buf = new StringBuilder();
        int end = 4;
        while (intVal.length() >= end) {
            String str = intVal.substring(0, end);
            buf.append(String.valueOf(Character.toChars(Integer.parseInt(str, 16))));
            intVal = intVal.substring(end);
        }
        return buf.toString();
    }

    private void printTable() {
        for (Integer key : hexToUni.keySet()) {
            System.out.printf("%6s=%s%n", Integer.toHexString(key), hexToUni.get(key));
        }
    }


    public static void main(String[] args) {
        HexToUnicode decoder = new HexToUnicode();
        decoder.readUnicode();
        // decoder.printTable();
        decoder.decodeString();
    }
}
