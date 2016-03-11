package mypdf.font;

import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;

/**
 * Created by ThomasE on 19.02.2016.
 */
public class MyColorModels {

    public static void main(String[] args) {
        ColorModel model = new DirectColorModel( 32, 0xff0000, 0xff00, 0xff, 0xff000000 );
        System.out.println(model);
        
    }

}
