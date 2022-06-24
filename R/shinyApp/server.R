# 使用beepr包添加声音
library(beepr)
fullTable<-totalMatrix()
cubes<-GnrCubes()
Gameon<-FALSE
server <- function(input, output, session) {
  totalscore<-0
  bgtable <-drawTable(totalscore)
  active<-reactiveVal(FALSE)
  observeEvent(input$pressedKey,{
    if (!is.null(input$keyPressed) && Gameon)
    {
      active(FALSE)
      code<-input$keyPressed
      # 重新设置按键映射
      if(code==37) ##Press Left
      {
        cubes<<-MoveLeft(cubes,fullTable)
        beep(10)
      }
      if(code==39) ##Press Right
      {
        cubes<<-MoveRight(cubes,fullTable)
        beep(10)
      }
      if(code==32) ##Press Spacebar
      {
        cubes<<-MoveDown(cubes,fullTable)
        beep(2)
      }
      if(code==68) ##Press A
      {
        # 逆时针旋转直接使用原有的rotate函数
        cubes<<-rotate(cubes,fullTable)
        #cubes<<-MoveRight(cubes)
        beep(10)
      }
      if(code==65) ##Press D
      {
        # 顺时针旋转则直接通过逆时针旋转三次实现
        i <- 1
        while (i <= 3) {
          cubes<<-rotate(cubes,fullTable)
          i <- i + 1
        }
        beep(10)
        #cubes<<-MoveRight(cubes)
      }
      active(TRUE)
    }
  })

  observe(
    {
      # 调整了更新时间吗，使得俄罗斯方块速度更快
      invalidateLater(1000, session)
      isolate({
        if(active())
        {
          bt<-UpdateTable(bgtable,cubes$cubesID)
          continueDrop<-checkNextBlock_y(cubes$cubesID,fullTable)
          if(continueDrop)
          {
            cubes$cubesID[,"y"]<<-cubes$cubesID[,"y"]-1
            rownames(cubes$cubeMatrix)<<-as.numeric(rownames(cubes$cubeMatrix))-1
          }
          else
          {
            for (i in 1:nrow(cubes$cubesID))
            {
              # 边界条件，30
              if(cubes$cubesID[i,"y"]>30)
                next()
              fullTable[as.character(cubes$cubesID[i,"y"]),as.character(cubes$cubesID[i,"x"])]<<-1
            }
            score<-GetScore(fullTable)
            if(score$score>0)
            {
              fullTable<<-score$tables
              totalscore<<-totalscore+score$score
              {
                output$ScorePanel <- renderText({paste0("Score: ",totalscore)   })
              }
            }
            bgtable<<-updateBackGround(fullTable,totalscore)
            if(endGame(fullTable))
            {
              active(FALSE)
              Gameon<<-FALSE
              output$LevelInfo<-renderText("Game Over")
            }
            cubes<<-GnrCubes()

            #active(FALSE)
          }
          output$plot <- renderPlot({
            bt
          })
        }
      })
    })


  output$plot <- renderPlot({
    invalidateLater(10000, session)
    bgtable
  })
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Time: ", Sys.time())
  })
  output$LevelInfo<-renderText("Level 1")
  output$ScorePanel <- renderText({"Score: 0"  })
  observeEvent(input$startGame,{active(TRUE)
    fullTable<<-totalMatrix()
    cubes<<-GnrCubes()
    Gameon<<-TRUE
    bgtable <<-drawTable(totalscore)})
  observeEvent(input$endGame,{
    active(FALSE)
    Gameon<<-FALSE
    })
  observeEvent(input$reset,{active(FALSE)
    output$LevelInfo<-renderText("Level 1")
    cubes<<-GnrCubes()
    bgtable <<-drawTable(totalscore)
    output$plot <- renderPlot({
      bgtable
    })})


}


