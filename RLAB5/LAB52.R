url<-read_html('https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/')


selector_name<-'a.post-list-item-title-link'
museum_names<-html_nodes(url, selector_name)%>%html_text()%>%as.vector()
museum_names <- gsub("\n", "", museum_names)
museum_names <- trimws(museum_names, which = "left")

selector_name<-'address.post-list-item-info'
museum_addres<-html_nodes(url, selector_name)%>%html_text()%>%as.vector()
museum_addres <- gsub("\n", "", museum_addres)
museum_addres <- trimws(museum_addres, which = "left")

selector_name<-'img.post-list-item-preview-image '
link <- "https://kudago.com/"
museum_image_link<- html_nodes(url, selector_name)%>%html_attr('src')
museum_image_link <- paste0(link, museum_image_link)

museums<-data.frame(museum_names[1:20], museum_addres[1:20], museum_image_link[1:20])

colnames(museums)<-c('Название музея', 'Адрес', 'Ссылка на фото')
show(museums)