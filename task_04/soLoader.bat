rem java -cp target/lession12-1.0.1-SNAPSHOT.jar org.mai.dep110.scala.stackoverflow.Main init --forse

rem java -cp target/lession12-1.0.1-SNAPSHOT.jar org.mai.dep110.scala.stackoverflow.Main clear --dropTables

rem java -cp target/lession12-1.0.1-SNAPSHOT.jar org.mai.dep110.scala.stackoverflow.Main load --path stackoverflow/

rem java -cp target/lession12-1.0.1-SNAPSHOT.jar org.mai.dep110.scala.stackoverflow.Main extract --query "select display_name, reputation, creation_date from users where views > 100" stackoverflow/