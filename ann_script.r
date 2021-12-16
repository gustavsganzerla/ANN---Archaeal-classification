df <- read.csv("/Users/gustavosganzerla/Desktop/project_arp/arp_a_novel_tool_to_predict_promoter_elements/ann/data/tata_bre/hvo_and_shuffled.csv", sep = ";")
df2 <- read.csv("/Users/gustavosganzerla/Desktop/project_arp/arp_a_novel_tool_to_predict_promoter_elements/ann/data/tata_bre/tk_and_shuffled.csv", sep = ";")


for (i in 2:2){
  for (j in 1:8) {
    j <- j*5
    for (k in 1:10) {
      index <- sample(1:nrow(ferritina_ann), round(0.9*nrow(ferritina_ann)))
      train.cv <- ferritina_ann[index,]
      test.cv <- ferritina_ann[-index,]
      
      index2 <- sample(1:nrow(df2), round(0.9*nrow(df2)))
      train2.cv <- df2[index,]
      test2.cv <- df2[-index2,]
      
      nn <- neuralnet(covid~DE_RESULTADO.x,
                                    hidden = c(5, 3),
                                    data = train.cv,
                                    err.fct = "ce",
                                    linear.output = FALSE,
                                    lifesign = "minimal",
                                    rep = j,
                                    stepmax = 200000)
      
      write.table(nn$net.result[1], paste("Hidden", i, "Epochs",j, "CV",k, ".txt"))
      
      
      
      
      if (file.info(paste("Hidden", 53, "Epochs",j, "CV",k, ".txt"))$size > 10) {#somente cria matrizes de redes que convergiram
        avaliar_treino <- compute(nn, train.cv)
        pred_treino <- ifelse(as.numeric(avaliar_treino$net.result)>0.5, 1, 0)
        matriz_confusao_treino <- table(pred_treino, train.cv$covid)
        write.table(matriz_confusao_treino, paste("Train_Matrix_Hidden_", i, "Epochs", j, "CV",k, ".txt"))


        avaliar_teste <- compute(nn, test.cv)
        pred_teste <- ifelse(as.numeric(avaliar_teste$net.result)>0.5, 1, 0)
        matriz_confusao_teste <- table(pred_teste, test.cv$covid)
        write.table(matriz_confusao_teste, paste("Test_Matrix_Hidden_", i, "Epochs", j, "CV",k, ".txt"))
        
        
        avaliar_teste2 <- compute(nn, test2.cv)
        pred_teste2 <- ifelse(as.numeric(avaliar_teste2$net.result)>0.5, 1, 0)
        matriz_confusao_teste2 <- table(pred_teste2, test2.cv$label)
        write.table(matriz_confusao_teste2, paste("Test_Matrix_Hidden_", i, "Epochs", j, "CV",k, ".txt"))
        

        
      }
      
      
    }
    
  }
}
