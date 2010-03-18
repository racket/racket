(module place '#%kernel
  (#%require '#%place)

  (#%provide place
             place-sleep
             place-wait 
             place-channel-send
             place-channel-recv
             place-channel?
             place?))
