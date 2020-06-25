package org.mai.dep810.stackoverflow

import scalikejdbc.config.DBs
import scopt.OptionParser

object Main {
  def main(args: Array[String]): Unit = {
    /*

    StackOverflowLoader 1.0
    Usage: StackOverflowLoader [load|clean|init|extract]

    Command: load [options]
    Load - это команда загрузки данных из файлов
      --path <value>           Путь к папке с файлами
      -a, --append             Не удалять данные при загрузке. По умолчанию данные будут перезатираться
    Command: clean [options]
    Удалить данные из базы данных
      -dt, --dropTables        Удалить таблицы
    Command: init [options]
    Создать таблицы
      -f, --forse              Пересоздать таблицы, если существуют
      Command: extract [options]
    Выгрузить данные в csv формате
      -q, --query              Запрос на выбор данных
      --file                   Файл, куда выгрузятся данные

    *\ */


    val parser = new OptionParser[Config]("StackOverflowLoader") {
      head("StackOverflowLoader", "1.0")
      cmd("load")
        .action((_, c) => c.copy(commandLoad = "load"))
        .text("Load - это команда загрузки данных из файлов")
        .children(
          opt[String]("path")
            .required()
            .action((f, c) => c.copy(file = f))
            .text("Путь к папке с файлами"),
          opt[Unit]("append")
            .abbr("a")
            .action((f, c) => c.copy(append = true))
            .text("Не удалять данные при загрозке. По умолчанию данные будут перезатираться"))
      cmd("clean")
        .action((_, c) => c.copy(commandClean = "clean"))
        .text("Удалить данные из базы данных")
        .children(
          opt[Unit]("dropTables")
            .abbr("dt")
            .action((_, c) => c.copy(dropTables = true))
            .text("Удалить таблицы"))
      cmd("init")
        .action((_, c) => c.copy(commandInit = "init"))
        .text("Создать таблицы")
        .children(
          opt[Unit]("forse")
            .abbr("f")
            .action((_, c) => c.copy(forse = true))
            .text("Пересоздать таблицы, если существуют")
        )
      cmd("extract")
        .action((_, c) => c.copy(commandExtract = "extract"))
        .text("Выгрузить данные в csv формате")
        .children(
          opt[String]("query")
            .abbr("q")
          .action((q,c) => c.copy(query = q))
              .text("Запрос на выбор данных"),
          opt[String]("file")
            .action((f,c)=>c.copy(file = f))
        )

      checkConfig { c =>
        if (c.commandInit.isEmpty && c.commandLoad.isEmpty && c.commandClean.isEmpty && c.commandExtract.isEmpty) failure("Нужно указать хотя бы одну комманду") else success
      }
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val dbOperations = DbOperations(Symbol("so"))
        if (!config.commandClean.isEmpty) {
          if (config.dropTables) {
            dbOperations.dropTableUser
            dbOperations.dropTablePost
            dbOperations.dropTableComment
          } else {
            dbOperations.clearTableUser
            dbOperations.clearTablePost
            dbOperations.clearTableComment
          }
        }
        if (!config.commandInit.isEmpty) {
          if (config.forse) {
            dbOperations.dropTableUser
            dbOperations.dropTablePost
            dbOperations.dropTableComment
          }
          dbOperations.createUserTable
          dbOperations.createPostTable
          dbOperations.createCommentTable
        }

        if (!config.commandLoad.isEmpty) {
          if (!config.append) {
            dbOperations.clearTableUser
            dbOperations.clearTablePost
            dbOperations.clearTableComment
          }

          val dbLoader = new DataLoader(config.commandLoad)
          val entities = dbLoader.loadData()
          val splittedEntities = Logic.splitEntities(entities)

          dbOperations.insertIntoUser(splittedEntities._1)
          dbOperations.insertIntoPost(splittedEntities._2)
          dbOperations.insertIntoComment(splittedEntities._3)
//          loadIrisToDB(IrisParser.loadFromFile(config.file))
        }

        if(!config.commandExtract.isEmpty){
          if(!config.query.isEmpty){
            if(!config.file.isEmpty){
              dbOperations.extract(config.query,config.file)
            }
            dbOperations.extract(config.query)
          }
        }

        DBs.closeAll()

      case None =>
    }

  }


}





