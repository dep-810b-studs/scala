package org.mai.dep110.scala.stackoverflow

object Logic {

  //obtain all commetns from entities
  def getComments(entities: Seq[Entity]): Seq[Comment] = {
    entities.foldLeft(Seq[Comment]()) {
      (accumulator, entity) => {
        entity match {
          case c: Comment => accumulator :+ c
          case _ => accumulator
        }
      }
    }
  }

  //split entities by type
  def splitEntities(entities: Seq[Entity]): (Seq[User], Seq[Post], Seq[Comment], Seq[Vote], Seq[Badge], Seq[Tag]) = {
    entities.foldLeft(Tuple6(Seq[User](), Seq[Post](), Seq[Comment](), Seq[Vote](), Seq[Badge](), Seq[Tag]())) {
      (accumulator, entity) => {
        entity match {
          case u: User => accumulator.copy(_1 = accumulator._1 :+ u)
          case p: Post => accumulator.copy(_2 = accumulator._2 :+ p)
          case c: Comment => accumulator.copy(_3 = accumulator._3 :+ c)
          case v: Vote => accumulator.copy(_4 = accumulator._4 :+ v)
          case b: Badge => accumulator.copy(_5 = accumulator._5 :+ b)
          case t: Tag => accumulator.copy(_6 = accumulator._6 :+ t)
        }
      }


    }
  }

  //populate fields owner, lastEditor, tags with particular users from Seq[Post] and tags from Seq[Tag]
  def enreachPosts(posts: Seq[Post], users: Seq[User], tags: Seq[Tag]): Seq[EnreachedPost] = {
    posts.map(post => EnreachedPost(
      post,
      users.find(user => user.id == post.ownerUserId).orNull,
      users.find(user => user.id == post.lastEditorUserId).orNull,
      tags.filter(tag => tag.wikiPostId == post.id)
    ))
  }

  //populate fields post and owner with particular post from Seq[Post] and user from Seq[User]
  def enreachComments(comments: Seq[Comment], posts: Seq[Post], users: Seq[User]): Seq[EnreachedComment] = {
    comments map (comment => EnreachedComment(
      comment,
      posts.find(post => post.id == comment.postId).orNull,
      users.find(user => user.id == comment.userId).orNull
    ))
  }

  //find all links (like http://example.com/examplePage) in aboutMe field
  def findAllUserLinks(users: Seq[User]): Seq[(User, Seq[String])] = {
    val emailChecker = ".*\\s(\\w+@\\w+\\.\\w+).*"
    users.map(user => (user, (user.about.split(" ").filter(str => str.matches(emailChecker))).toSeq))
  }

  //find all users with the reputation bigger then reputationLImit with particular badge
  def findTopUsersByBadge(users: Seq[User], badges: Seq[Badge], badgeName: String, reputationLimit: Int): Seq[User] = {
    users.filter(user => user.id == (badges.find(badge => badge.name == badgeName).orNull).userId && user.reputation > reputationLimit)
  }

}
case class EnreachedPost(
                        post: Post,
                        owner: User,
                        lastEditor: User,
                        tags: Seq[Tag]
                        )

case class EnreachedComment(
                          comment: Comment,
                          post: Post,
                          owner: User
                        )
