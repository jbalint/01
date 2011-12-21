import java.util.*;
import java.util.regex.*;
import java.sql.*;
import java.io.*;
import java.lang.reflect.*;

/**
 * Util to update db records with a given cursor.
 *
 * Define a procedure "p"(with param ResultSet rs) and a query "SQL_p"(String),
 * both as class variables. Query will be ran and procedure called once for each
 * row.
 *
 * javac -cp lib/mysql-connector-java-5.1.17-bin.jar DbUtil.java && java -cp .:lib/mysql-connector-java-5.1.17-bin.jar DbUtil
 */
public class DbUtil {

    /**
     * Code to hack mysql driver to allow updating when derived fields are used.
     */
    static void procedure_that_uses_derived_fields(ResultSet rs) throws Exception {
	/* hack for mysql (doesnt allow updating a resultset with a computed field in it) */
	Field f = com.mysql.jdbc.UpdatableResultSet.class.getDeclaredField("isUpdatable");
	f.setAccessible(true);
	f.setBoolean(rs, true);
	Field f_fields = com.mysql.jdbc.ResultSetImpl.class.getDeclaredField("fields");
	f_fields.setAccessible(true);
	com.mysql.jdbc.Field mysql_fields[] = (com.mysql.jdbc.Field[])f_fields.get(rs);
	// non-derived fields should be first
	f_fields.set(rs, Arrays.copyOfRange(mysql_fields, 0, 2));

	// do stuff

	// check update query
	    Field s = com.mysql.jdbc.UpdatableResultSet.class.getDeclaredField("updateSQL");
	    s.setAccessible(true);
	    String sql = (String)s.get(rs);
	    System.out.println("sql: " + sql);

	// flip it back so we can access the 3rd field next time
	f_fields.set(rs, mysql_fields);
    }

    /**
     * Example procedure. Run: java DbUtil test
     */
    static void test(ResultSet rs) throws Exception {
    	System.out.println("got row: " + rs.getString(1));
    }
    static String SQL_test = "select 1";

    public static void main(String args[]) throws Exception {
	if (args.length != 1) {
	    System.err.println("Usage: {0} <procedureName>");
	    System.exit(1);
	}
	Field field = DbUtil.class.getDeclaredField("SQL_" + args[0]);
	field.setAccessible(true);
	Method method = DbUtil.class.getDeclaredMethod(args[0], ResultSet.class);
	method.setAccessible(true);
	Class.forName("com.mysql.jdbc.Driver").newInstance();
	int rowsRetrieved = 0, rowsUpdated = 0;
	Connection conn = DriverManager.getConnection(
				"jdbc:mysql://localhost/agora_scrape",
				"agora_scrape", "kkagora0127");
	Statement stmt = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY,
					      ResultSet.CONCUR_UPDATABLE);
	ResultSet rs = stmt.executeQuery((String)field.get(null));
	boolean hasRowUpdated = true;
	while(rs.next()) {
	    rowsRetrieved++;
	    method.invoke(null, rs);
	    try {
	    	if (hasRowUpdated && rs.rowUpdated())
	    	    rowsUpdated++;
	    } catch(java.sql.SQLFeatureNotSupportedException ex) {
	    	hasRowUpdated = false;
	    	continue;
	    }
	}
	rs.close();
	stmt.close();
	conn.close();
	System.out.println("Rows retrieved: " + rowsRetrieved);
	System.out.println("Rows updated: " + (hasRowUpdated ? rowsUpdated : "<ResultSet.rowUpdated() not supported>"));
    }
}
