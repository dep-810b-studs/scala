package org.mai.dep810.stackoverflow

object Logic {
  def splitEntities(entities: Seq[Entity]): (Seq[User], Seq[Post], Seq[Comment]) = {
    entities.foldLeft((Seq[User](), Seq[Post](), Seq[Comment]())) { case ((users, posts, comments), entity) =>
      entity match {
        case u: User => (u +: users, posts, comments)
        case p: Post => (users, p +: posts, comments)
        case c: Comment => (users, posts, c +: comments)
      }
    }
  }
}

  case class Config (
                      commandLoad: String = "",
                      commandClean: String = "",
                      commandInit: String = "",
                      commandExtract: String ="",
                      path: String = "",
                      file: String = "",
                      query: String ="",
                      append: Boolean = false,
                      dropTables: Boolean = false,
                      forse: Boolean = false)
