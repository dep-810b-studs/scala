package org.mai.dep810.stackoverflow

import java.io._
import java.time.LocalDateTime

import scalikejdbc.{NamedDB, _}
import scalikejdbc.config.DBs

import scala.xml.XML

class DataLoader(basePath: String) {

  private[this] val filesToFunc = Map[String, String => Seq[Entity]](
    "Users.xml" -> loadUsers,
    "Posts.xml" -> loadPosts,
    "Comments.xml" -> loadComments
  )

  def loadData(): scala.collection.Seq[Entity] = {
    filesToFunc
      .flatMap{
        case (file, func) =>
          val path = basePath+"/"+file
          func(path)
      }.toSeq
  }

  private[this] def loadUsers(path: String): Seq[User] = {
    val usersXml = XML.load(path)

    for(
      userRow <- usersXml \\ "row"
    ) yield {
      User(
        (userRow \@ "Id").toInt,
        userRow \@ "DisplayName",
        userRow \@ "location",
        userRow \@ "AboutMe",
        matchInt(userRow \@ "Reputation"),
        matchInt(userRow \@ "Views"),
        matchInt(userRow \@ "UpVotes"),
        matchInt(userRow \@ "DownVotes"),
        matchInt(userRow \@ "AccountId"),
        parseDate(userRow \@ "CreationDate"),
        parseDate(userRow \@ "LastAccessDate")
      )
    }
  }

  private[this] def loadPosts(path: String): Seq[Post] = {
    val postsXml = XML.load(path)

    for(
      postRow <- postsXml \\ "row"
    ) yield {
      Post(
        matchInt(postRow \@ "Id"),
        postRow \@ "Title",
        postRow \@ "Body",
        matchInt(postRow \@ "Score"),
        matchInt(postRow \@ "ViewCount"),
        matchInt(postRow \@ "AnswerCount"),
        matchInt(postRow \@ "CommentCount"),
        matchInt(postRow \@ "OwnerUserId"),
        matchInt(postRow \@ "LastEditorUserId"),
        matchInt(postRow \@ "AcceptedAnswerId"),
        parseDate(postRow \@ "CreationDate"),
        parseDate(postRow \@ "LastEditDate"),
        parseDate(postRow \@ "LastActivityDate")
//        (postRow \@ "Tags").stripPrefix("&lt;").stripSuffix("&gt;").split("&gt;&lt;").toSeq
      )
    }
  }

  private[this] def loadComments(path: String): Seq[Comment] = {
    val commentsXml = XML.load(path)

    for(
      row <- commentsXml \\ "row"
    ) yield {
      Comment(
        matchInt(row \@ "Id"),
        matchInt(row \@ "PostId"),
        matchInt(row \@ "Score"),
        row \@ "Text",
//        parseDate(row \@ "CreationDate"),
        matchInt(row \@ "UserId")
      )
    }
  }

  private[this] def matchInt(s: String): Int = {
    val intMatch = "(\\d+)".r
    s match {
      case intMatch(i) => i.toInt
      case _ => Int.MinValue
    }
  }

  private[this] def parseDate(s: String): LocalDateTime = {
    if(s == "")
      null
    else
      LocalDateTime.parse(s)
  }
}

abstract class Entity(id: Int)

case class User(
                 id: Int,
                 displayName: String,
                 location: String,
                 about: String,
                 reputation: Int,
                 views: Int,
                 upVotes: Int,
                 downVotes: Int,
                 accountId: Int,
                 creationDate: LocalDateTime,
                 lastAccessDate: LocalDateTime) extends Entity(id)

case class Post(
                 id: Int,
                 title: String,
                 body: String,
                 score: Int,
                 viewCount: Int,
                 answerCount: Int,
                 commentCount: Int,
                 ownerUserId: Int,
                 lastEditorUserId: Int,
                 acceptedAnswerId: Int,
                 creationDate: LocalDateTime,
                 lastEditDate: LocalDateTime,
                 lastActivityDate: LocalDateTime) extends Entity(id)

case class Comment(
                    id: Int,
                    postId: Int,
                    score: Int,
                    text: String,
//                    creationDate: LocalDateTime,
                    userId: Int) extends Entity(id)

object User extends SQLSyntaxSupport[User]{
  override val tableName: String = "so_user"
  override def connectionPoolName: Any = Symbol("so")

  def apply(obj : ResultName[User])(rs : WrappedResultSet): Unit = {
    new User(
      rs.int(obj.id),
      rs.string(obj.displayName),
      rs.string(obj.location),
      rs.string(obj.about),
      rs.int(obj.reputation),
      rs.int(obj.views),
      rs.int(obj.upVotes),
      rs.int(obj.downVotes),
      rs.int(obj.accountId),
      rs.localDateTime(obj.creationDate),
      rs.localDateTime(obj.lastAccessDate))
  }
}

object Post extends SQLSyntaxSupport[Post] {
  override val tableName: String = "so_post"
  override def connectionPoolName: Any = Symbol("so")

  def apply(post: ResultName[Post])(rs: WrappedResultSet): Unit = {
    new Post(
      rs.int(post.id),
      rs.string(post.title),
      rs.string(post.body),
      rs.int(post.score),
      rs.int(post.viewCount),
      rs.int(post.answerCount),
      rs.int(post.commentCount),
      rs.int(post.ownerUserId),
      rs.int(post.lastEditorUserId),
      rs.int(post.acceptedAnswerId),
      rs.localDateTime(post.creationDate),
      rs.localDateTime(post.lastEditDate),
      rs.localDateTime(post.lastActivityDate)
    )
  }
}

object Comment extends SQLSyntaxSupport[Comment] {
  override val tableName: String = "so_comment"
  override def connectionPoolName: Any = Symbol("so")

  def apply(comment : Comment)(rs: WrappedResultSet): Unit ={
    new Comment(
      rs.int(comment.id),
      rs.int(comment.postId),
      rs.int(comment.score),
      rs.string(comment.text),
//      rs.localDateTime(comment.creationDate),
      rs.int(comment.userId)
    )
  }
}

case class DbOperations(dbConfig : Symbol) {
  DBs.setup(dbConfig)

  def createUserTable = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"""
         create table if not exists so_user (
             id  int,
             display_name varchar (100),
             location varchar (100),
             about clob (10K),
             reputation int,
             views int,
             up_votes int,
             down_votes int,
             account_id int,
             creation_date timestamp,
             last_access_date timestamp
         );
       """.execute.apply

    println(new java.io.File(".").getCanonicalPath)
  }

  def createPostTable = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"""
      create table  if not exists so_post(
       id int,
       title clob (10K),
       body clob (100K),
       score int,
       view_count int,
       answer_count int,
       comment_count int,
       owner_user_id int,
       last_editor_user_id int,
       accepted_answer_id int,
       creation_date timestamp,
       last_edit_date timestamp,
       last_activity_date timestamp
      )
      """.execute.apply
  }

  def createCommentTable = NamedDB(dbConfig).autoCommit{ implicit session =>
    sql"""
                create table if not exists so_comment(
                id int,
                post_id int,
                score int,
                text clob (10K),
                creation_date timestamp,
                user_id int
                )
            """.execute.apply()
  }

  def dropTableUser: Unit = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"drop table if exists so_user".execute.apply()
  }
  def dropTablePost: Unit = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"drop table if exists so_post".execute.apply()
  }
  def dropTableComment: Unit = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"drop table if exists so_comment".execute.apply()
  }

  def clearTableUser = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"delete from so_user".update.apply()
  }
  def clearTablePost = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"delete from so_post".update.apply()
  }
  def clearTableComment = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"delete from so_comment".update.apply()
  }

  def insertIntoUser (users : Seq[User]) = NamedDB(dbConfig).autoCommit { implicit session =>
    val u = User.column
    users.foreach { user =>
      withSQL(
        insert.into(User).namedValues(
          u.id -> user.id,
          u.displayName -> user.displayName,
          u.location -> user.location,
          u.about -> user.about,
          u.reputation -> user.reputation,
          u.views -> user.views,
          u.upVotes -> user.upVotes,
          u.downVotes -> user.downVotes,
          u.accountId -> user.accountId,
          u.creationDate -> user.creationDate,
          u.lastAccessDate -> user.lastAccessDate)
      ).update.apply
    }
  }
  def insertIntoPost (posts : Seq[Post]) = NamedDB(dbConfig).autoCommit{implicit  session =>
    val p = Post.column
    posts.foreach{post =>
      withSQL(
        insert.into(Post).namedValues(
          p.id-> post.id,
          p.title-> post.title,
          p.body-> post.body,
          p.score -> post.score,
          p.viewCount -> post.viewCount,
          p.answerCount -> post.answerCount,
          p.commentCount -> post.commentCount,
          p.ownerUserId -> post.ownerUserId,
          p.lastEditorUserId -> post.lastEditorUserId,
          p.acceptedAnswerId -> post.acceptedAnswerId,
          p.creationDate -> post.creationDate,
          p.lastEditDate -> post.lastEditDate,
          p.lastActivityDate -> post.lastActivityDate
        )
      ).update.apply
    }
  }
  def insertIntoComment (comments : Seq[Comment]): Unit = NamedDB(dbConfig).autoCommit{ implicit session =>
    val c = Comment.column
    comments.foreach{comment =>
      withSQL(
        insert.into(Comment).namedValues(
          c.id -> comment.id,
          c.postId -> comment.postId,
          c.score -> comment.score,
          c.text -> comment.text,
//          c.creationDate -> comment.creationDate,
          c.userId -> comment.userId
      )).update.apply
    }
  }

  def selectFromComment: String = NamedDB(dbConfig).autoCommit{ implicit session =>
   val comment = sql" select text from so_comment where score = 1".map(rs => rs.string("text"))
      .single()
      .apply().orNull

    println(comment)

    comment
  }

  def checkIfTablesExist: Boolean = NamedDB(dbConfig).autoCommit { implicit session =>
    sql"select count(*) as cnt from information_schema.tables where table_name = 'so_comment'".map(rs => rs.int("cnt"))
      .single
      .apply() match {
      case Some(c) => c == 1
      case None => false
    }
  }

  def extract(query : String, filename :String = null)  = {
    val connection = NamedDB(dbConfig).conn
    val statment = connection.createStatement
    val resultSet = statment.executeQuery(query)
    var fileName = filename

    if(fileName == null){
      fileName = "./output.csv"
    }

    val columns = 1 to resultSet.getMetaData.getColumnCount map resultSet.getMetaData.getColumnName
    val printWriter = new PrintWriter(fileName)

    for(i <- 1 to resultSet.getMetaData.getColumnCount){
      if(i >1){
        printWriter.print("; ")
      }
    printWriter.print(resultSet.getMetaData.getColumnName(i))
    }

    printWriter.println

    columns.foreach(column => printWriter.print(column))
    while (resultSet.next()){
      for( i <- 1 to resultSet.getMetaData.getColumnCount) {
          if(i > 1){
            printWriter.print("; ")
          }
          printWriter.print(resultSet.getString(i))
      }
      printWriter.println
    }


//    printWriter.println()

    printWriter.close()



  }
}
