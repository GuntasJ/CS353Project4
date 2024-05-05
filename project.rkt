#lang racket

;Helper wrapper function that allows to get input from any platform
(define (get-user-input)
    (read-line (current-input-port) 'any))

;sees a string is equal to a symbol. The Symbol is in all uppercase. The string is case insensitive
(define/contract (string-symbol-ci=? str symbol)
    (string? symbol? . -> . boolean?)
    (equal? (string->symbol (string-upcase str)) symbol))

;items & helper methods
;Defines the different items that are present in the map
;Item = (item-id item-name item-description)
(define torch '(TORCH "Torch" "A torch with flames that still yet burn"))
(define key-1 '(KEY-1 "Key #1" "An ancient key with marks from a language you don't know"))
(define key-2 '(KEY-2 "Key #2" "On it reads: jiondiyaa'n nu ki saar moiyaa'n di"))
(define key-3 '(KEY-3 "Key #3" "Escape from this lies oh so close away; escape from the rest though..."))

;getter
(define (get-item-id item)
    (first item))
;getter
(define (get-item-name item)
    (second item))
;getter
(define (get-item-description item)
    (third item))
;combines the item name and description
(define (get-item-print-string item)
    (~a (get-item-name item) ": " (get-item-description item)))

;helper function that takes the name of the item and returns the item
(define (get-item-by-name name)
    (cond
        [(string-ci=? name "Torch") torch]
        [(string-ci=? name "Key #1") key-1]
        [(string-ci=? name "Key #2") key-2]
        [(string-ci=? name "Key #3") key-3]))

;position & helper functions
;helper function to create a position
(define (create-position index position-id position-name adj-positions items)
    `(,index ,position-id ,position-name ,adj-positions ,items))

;position = (index position-tag position-name (adj-pos) (items))
(define position-1  `(0 POS-1 "Entrance" ( (POS-5 SOUTH) ) ()))
(define position-2  `(1 POS-2 "Position 2" ( (POS-5 EAST) (POS-3 SOUTH) ) ()))
(define position-3  `(2 POS-3 "Position 3" ( (POS-2 NORTH) (POS-6 EAST) (POS-4 SOUTH) ) ()))
(define position-4  `(3 POS-4 "Position 4" ( (POS-3 NORTH) ) (,key-1)))
(define position-5  `(4 POS-5 "Position 5" ( (POS-1 NORTH) (POS-7 EAST) (POS-6 SOUTH) (POS-2 WEST)) (,torch)))
(define position-6  `(5 POS-6 "Position 6" ( (POS-3 WEST) (POS-5 NORTH)) ()))
(define position-7  `(6 POS-7 "Position 7" ( (POS-5 WEST) (POS-8 SOUTH)) (,key-2)))
(define position-8  `(7 POS-8 "Position 8" ( (POS-7 NORTH) (POS-11 EAST) (POS-9 SOUTH)) ()))
(define position-9  `(8 POS-9 "Position 9" ( (POS-8 NORTH) (POS-12 EAST)) ()))
(define position-10 `(9 POS-10 "Exit" ( (POS-11 SOUTH)) ()))
(define position-11 `(10 POS-11 "Position 11" ( (POS-10 NORTH) (POS-8 WEST)) ()))
(define position-12 `(11 POS-12 "Position 12" ( (POS-9 WEST)) (,key-3)))

;getter
(define (get-position-index position)
    (first position))
;getter
(define (get-position-id position)
    (second position))
;getter
(define (get-position-name position)
    (third position))
;getter
;returns list of list
(define (get-adj-position-descriptors position)
    (fourth position))
;getter
;returns list
(define (get-items-from-position position)
    (fifth position))

;helper function that takes in the game-map and position-id and return the position from that
;Position-id is assumed to be a valid entry
(define (get-position-by-id position-id game-map)
    (first (filter (λ (position) (equal? (get-position-id position) position-id)) game-map)))

;gets the adj position of a posiition based on direction.
;Direction is assumed to be valid
(define (get-adj-position-by-direction position direction game-map)
    (define target-position-id (first (first
                                        (filter 
                                            (λ (adj-pos-descriptor) (equal? (second adj-pos-descriptor) direction)) 
                                            (get-adj-position-descriptors position)))))
    (get-position-by-id target-position-id game-map))
    
;creates a copy of a position with the item added to the positions items list
(define (add-item-to-position item position)
    (create-position 
        (get-position-index position) 
        (get-position-id position) 
        (get-position-name position)
        (get-adj-position-descriptors position)
        (append (get-items-from-position position) `(,item))))

;creates a copy of a position with the item removed from the postitions list
(define (remove-item-from-position item position)
    (create-position
        (get-position-index position)
        (get-position-id position)
        (get-position-name position)
        (get-adj-position-descriptors position)
        (remove item (get-items-from-position position))))

;replaces an old-position with a new one and has the change reflected in the game-state
(define (replace-position old-position new-position game-state)
    (define game-map (get-game-map game-state))
    (define new-game-map (list-set game-map (get-position-index old-position) new-position))
    (cond
        [(equal? (get-current-position game-state) old-position) `(,new-position ,new-game-map)]
        [else `(,(get-current-position game-state) ,new-game-map)]))

;the container list that holds all positions
(define game-map `(
    ,position-1
    ,position-2
    ,position-3
    ,position-4
    ,position-5
    ,position-6
    ,position-7
    ,position-8
    ,position-9
    ,position-10
    ,position-11
    ,position-12))

;game-state = (current-position game-map)
;current-positition is held in the game state not the player state for ease-of-use
(define game-state `(,position-1 ,game-map))

;getter
(define (get-current-position game-state)
    (first game-state))
;getter
;returns a list
(define (get-game-map game-state)
    (second game-state))

;helper methods
;takes an item as input and sees if that item has been discovered from a seperate discovered-list
;discovered list is a list of true or falses with each index corrasponding to an item. 
(define (item-discovered? item discovered-list)
    (cond
        [(equal? (get-item-id item) 'TORCH) (first discovered-list)]
        [(equal? (get-item-id item) 'KEY-1) (second discovered-list)]
        [(equal? (get-item-id item) 'KEY-2) (third discovered-list)]
        [(equal? (get-item-id item) 'KEY-3) (fourth discovered-list)]))

;modifies the discovered list and modifies it if an item is has not been discovered yet
(define (discover-item item discovered-list)    
    (cond
        [(equal? (get-item-id item) 'TORCH) (flatten `(#t ,(rest discovered-list)))]
        [(equal? (get-item-id item) 'KEY-1) (flatten `(,(first discovered-list) #t ,(drop discovered-list 2)))]
        [(equal? (get-item-id item) 'KEY-2) (flatten `(,(take discovered-list 2) #t ,(last discovered-list)))]
        [(equal? (get-item-id item) 'KEY-3) (flatten `(,(take discovered-list 3) #t))]))

;helper function that converts the direction symbol to a string that has the first letter captial rest lowercase
(define (get-direction-name direction)
    (cond
        [(equal? direction 'NORTH) "North"]
        [(equal? direction 'EAST) "East"]
        [(equal? direction 'SOUTH) "South"]
        [(equal? direction 'WEST) "West"]))

;user options
;displays all the different options that the user has
(define (display-user-options)
    (displayln "Enter 'T' to travel to another area")
    (displayln "Enter 'R' to report the current location")
    (displayln "Enter 'D' to drop an item")
    (displayln "Enter 'P' to pick up an item")
    (displayln "Enter 'I' to display your inventory")
    (displayln "Enter 'S' to search the current area")
    (displayln "Enter 'E' to describe the current area (this will show you if any found items are available")
    (displayln "Enter 'H' for help. This will bring up where you are, your inventory, and what actions are avaiable\n"))

;Returns a new gamestate with the updated current position
;user-choice is assumed to be valid
(define (move-to user-choice game-state)
    (define current-position (get-current-position game-state))
    (define game-map (get-game-map game-state))
    (define new-position (get-adj-position-by-direction current-position (string->symbol (string-upcase user-choice)) game-map))
    `(,new-position ,game-map))

;updates the gamestate with the new current location
(define (travel game-state)
    (displayln "\nThe following paths are available:\n")
    (define adj-position-descriptors (get-adj-position-descriptors (get-current-position game-state)))
    (for-each displayln (map 
                            (λ (adj-pos-descriptor) (get-direction-name (second adj-pos-descriptor))) 
                            adj-position-descriptors))

    (displayln "\nEnter the path to travel to (enter the cardinal direction):\n")
    (define user-choice (get-user-input))
    (cond
        [(ormap 
            (λ (adj-position-descriptor) (string-symbol-ci=? user-choice (second adj-position-descriptor))) 
            adj-position-descriptors)
            (move-to user-choice game-state)]
        [else 
            (displayln "That path is not avaiable. Please try again\n")
            game-state]))

;prints out where the current location is
(define (report-current-location game-state)
    (displayln (~a "\nYou are at " (get-position-name (get-current-position game-state)) "\n")))

;displays all the items that have been found and are avaiable at the current position
(define (describe-current-location game-state discovered-list)
    (define current-position (get-current-position game-state))
    (define found-items (filter (λ (item) (item-discovered? item discovered-list)) (get-items-from-position current-position)))
    (cond 
        [(empty? found-items)
            (displayln "From your initial gaze, no items strike your eye")]
        [else
            (displayln "From your initial gaze around the room, the following items strike your eye\n")
            (for-each (λ (item) (displayln (get-item-print-string item))) found-items)
            (display "")]))

;displays the inventory if it has any items in it
(define (list-inventory inventory)
    (cond
        [(empty? inventory)
             (displayln "Your inventory is emtpy\n")]
        [else
            (displayln "Your carry the following items:\n")
            (for-each (λ (item) (displayln (get-item-print-string item))) inventory)
            (displayln "")]))

;picks up the item from current position, removing it from the game state and putting it in inventory
(define (pick-up-item game-state inventory discovered-list)
    (displayln "Enter the name of the item that you wish to pick up:\n")
    (define items (get-items-from-position (get-current-position game-state)))
    ;(displayln (~a "items: " items))
    (define found-items (filter (λ (item) (item-discovered? item discovered-list)) items))
    ;(displayln (~a "found-items: " found-items))
    

    (define user-choice (get-user-input))
    ;check if user choice is valid
    (cond  
        [(empty? (filter (λ (item) (string-ci=? user-choice (get-item-name item))) found-items))
            (displayln "\nThat item does not exist or has not been discovered\n")
            `(,game-state ,inventory)]
        [else 
            (define current-position (get-current-position game-state))
            (define item (get-item-by-name user-choice))
            (define updated-inventory (append inventory `(,item)))
            (define updated-position (remove-item-from-position item current-position))
            (define updated-game-state (replace-position current-position updated-position game-state))
            ;(println (get-current-position updated-game-state))
            (displayln (~a "You picked up the following item: " (get-item-print-string item) "\n"))
            `(,updated-game-state ,updated-inventory)]))

;drops the item, removing it from inventory and putting it in the game-state
(define (drop-item game-state inventory)
    (displayln "Enter the name of the item that you wish to drop\n")
    (define user-choice (get-user-input))

    (cond
        [(empty? (filter (λ (item) (string-ci=? user-choice (get-item-name item))) inventory))
            (displayln "\nThat item does not exist or has not been discovered\n")
            `(,game-state ,inventory)]
        [else  
            (define current-position (get-current-position game-state))
            (define item (get-item-by-name user-choice))
            (define updated-inventory (remove item inventory))
            (define updated-position (add-item-to-position item current-position))
            (define updated-game-state (replace-position current-position updated-position game-state))
            (displayln (~a "You dropped the following item: " (get-item-print-string item)))
            `(,updated-game-state ,updated-inventory)]))
            
;searches for items in current position that have not been found and discovered them
(define (search game-state discovered-list) 
    (define items (get-items-from-position (get-current-position game-state)))
    (define unfound-items (filter (λ (item) (not (item-discovered? item discovered-list))) items))
    (cond
        [(empty? unfound-items)
            (displayln "There are no new items to be found here\n")
            discovered-list]
        [else 
            (displayln "The following items have been uncovered:\n")
            (for-each (λ (unfound-item) (println (get-item-print-string unfound-item))) unfound-items)
            (displayln "")
            (foldl discover-item discovered-list unfound-items)]))

;gives the user an overview of what is going on and what they can do
(define (help game-state inventory)
    ;Where the player is
    (report-current-location game-state)

    ;your inventory
    (list-inventory inventory)

    ;what actions are avaiable
    (display "You may travel to another area,")
    (display " report the current location,")
    (display " drop an item,")
    (display " pick up an item,")
    (display " display your inventory,")
    (display " search the current area,")
    (displayln " and describe the current area.\n"))

;game ends when all 3 keys are dropped at the end
(define (game-ended? game-state)
    (define ending-position (get-position-by-id 'POS-10 (get-game-map game-state)))
    (define items (get-items-from-position ending-position))
    (cond  
        [(and 
            (ormap (λ (item) (equal? (get-item-id item) 'KEY-1)) items)
            (ormap (λ (item) (equal? (get-item-id item) 'KEY-2)) items)
            (ormap (λ (item) (equal? (get-item-id item) 'KEY-3)) items))
            #t]
        [else 
            #f]))

;main function to plavy game
(define (play-game game-state [inventory '()])
    (displayln "
                You have entered the dungeon of Khorasur the Zaalim. 
                You must find the three keys and the exit to escape.
                Present them to the exit, and you shall know freedom.
                Fail to do so and well... \n")
    (define (play-game-loop game-state inventory [discovered-list '(#f #f #f #f)])
        (unless (game-ended? game-state)
            (display-user-options)
            (define user-choice (get-user-input))
            (cond
                [(string-ci=? user-choice "T") 
                    (play-game-loop (travel game-state) inventory discovered-list)]

                [(string-ci=? user-choice "R")
                    (report-current-location game-state)
                    (play-game-loop game-state inventory discovered-list)]

                [(string-ci=? user-choice "S") 
                    (play-game-loop game-state inventory (search game-state discovered-list))]

                [(string-ci=? user-choice "P") 
                    (define result (pick-up-item game-state inventory discovered-list))
                    (play-game-loop (first result) (second result) discovered-list)]

                [(string-ci=? user-choice "I") 
                    (list-inventory inventory)
                    (play-game-loop game-state inventory discovered-list)]

                [(string-ci=? user-choice "D")
                    (define result (drop-item game-state inventory))
                    (play-game-loop (first result) (second result) discovered-list)]
                
                [(string-ci=? user-choice "E")
                    (describe-current-location game-state discovered-list)
                    (play-game-loop game-state inventory discovered-list)]

                [(string-ci=? user-choice "H")
                    (help game-state inventory)
                    (play-game-loop game-state inventory discovered-list)]

                [else 
                    (displayln "Invalid option! Please enter again\n")
                    (play-game-loop game-state inventory)])))
    (play-game-loop game-state inventory)
    (displayln "You have escaped the dungeon and evaded Khorasur's wrath!\n")
    (displayln "Do you wanna play again? (Y/N): \n")
    (define play-again-response (get-user-input))
    (if (string-ci=? play-again-response "Y")
        (play-game game-state)
        (displayln "Game over!")))

(play-game game-state)
        

;f f f f
;t f f f


