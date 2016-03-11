package database;

import java.sql.*;

/**
 * Created by ThomasE on 22.02.2016.
 */
public class DatabaseTest {

    public static void main(String[] args) {
        try {
            Class.forName("com.mysql.jdbc.Driver");
            Connection connect = DriverManager
                    .getConnection("jdbc:mysql://localhost/inethelpdesk?user=root");
            Statement statement = connect.createStatement();
            ResultSet resultSet = statement.executeQuery( "show tables" );
            System.out.println(resultSet);



        } catch ( Exception e ) {
            e.printStackTrace();
        }
    }
}
