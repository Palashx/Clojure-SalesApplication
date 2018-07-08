
; Reading from text file 

(def custText (slurp "cust.txt"))
(def prodText (slurp "prod.txt"))
(def salesText (slurp "sales.txt"))

; Making a list

(def custList (list* (clojure.string/split custText #"\n")))
(def prodList (list* (clojure.string/split prodText #"\n")))
(def salesList (list* (clojure.string/split salesText #"\n")))

; Making a map for customer
(def customerMap (hash-map))
(def mapCustomerName (hash-map))

; Making a map for product
(def productMap (hash-map))
(def mapProductItem (hash-map))

; Making a map for product
(def mapSales (hash-map))


; Making customer map
(dotimes [n (count custList)] 
	(def customerElement (nth custList n))
	(def splittingCustomer (list* (clojure.string/split customerElement #"\|")))
	(def keyMap (nth splittingCustomer 0))
	(def nameMap (nth splittingCustomer 1))
	(def customerAddress (nth splittingCustomer 2))
	(def customerNumber (nth splittingCustomer 3))
	(def customerAttribute (str nameMap " " customerAddress " " customerNumber))	
	(def pseudoMap (hash-map keyMap customerAttribute))
	(def customerMap (merge customerMap pseudoMap))


	(def tempCustomerMap (hash-map keyMap nameMap))
	(def mapCustomerName (merge mapCustomerName tempCustomerMap))

)

; Making product map

(dotimes [n (count prodList)] 
	(def productElement (nth prodList n))
	(def splittingProduct (list* (clojure.string/split productElement #"\|")))
	(def keyMap (nth splittingProduct 0))
	(def itemName (nth splittingProduct 1))
	(def itemCost (nth splittingProduct 2))
	(def itemAttribute(str itemName " " itemCost))
	(def pseudoMap (hash-map keyMap itemAttribute))
	(def productMap (merge productMap pseudoMap))

	(def tempProductMap (hash-map keyMap itemName))
	(def mapProductItem (merge mapProductItem tempProductMap))
)

; Cast String to int 
(defn parse-int [x]
  (Integer. (re-find  #"\d+" x ))
)
; Making sales map

(dotimes [n (count salesList)] 
	(def salesElement (nth salesList n))
	(def splittingSales (list* (clojure.string/split salesElement #"\|")))
	(def keyMap (parse-int (nth splittingSales 0)))
	(def custID (nth splittingSales 1))
	(def productID (nth splittingSales 2))
	(def numberSale (nth splittingSales 3))
	(def saleAttribute (str custID " " productID " " numberSale))	
	(def pseudoMap (hash-map keyMap saleAttribute))
	(def mapSales (merge mapSales pseudoMap))
)



(defn promptInput []
	(read-line) 
)

; Sorting maps
(def customerSortedMap (into (sorted-map) customerMap))
(def productSortedMap (into (sorted-map) productMap))
(def salesSortedMap (into (sorted-map) mapSales))

; Fuction for option1
(def printFinalCustomerMap (hash-map))
(defn option1 []
	
	(dotimes [n (count custList)] 
	(def printCustomerElement (nth custList n))
	(def printSplittingCustomer (list* (clojure.string/split printCustomerElement #"\|")))
	(def printKeyMap (parse-int (nth printSplittingCustomer 0)))
	(def printNameMap (nth printSplittingCustomer 1))
	(def printCustomerAddress (nth printSplittingCustomer 2))
	(def printCustomerNumber (nth printSplittingCustomer 3))
	(def printCustomerAttribute (str ": " printNameMap ", " printCustomerAddress ", " printCustomerNumber))
	(def printCustomerMap (hash-map printKeyMap printCustomerAttribute))
	(def printFinalCustomerMap (merge printFinalCustomerMap printCustomerMap)))

	(def printCustomerSortedMap (into (sorted-map) printFinalCustomerMap))
	
	(def printCustomer 
  		(doseq [i (sort-by first < printCustomerSortedMap)] 
  			(println (key i) (val i))
  		)
  	)
 )

; Fuction for option2
(def printFinalProductMap (hash-map))
(defn option2 []

	(dotimes [n (count prodList)] 
	(def printProductElement (nth prodList n))
	(def printSplittingProduct (list* (clojure.string/split printProductElement #"\|")))
	(def printKeyMap (parse-int( nth printSplittingProduct 0)))
	(def printNameMap (nth printSplittingProduct 1))
	(def printProductCount (nth printSplittingProduct 2))
	(def printProductAttribute (str ": " printNameMap ", " printProductCount))
	(def printProductMap (hash-map printKeyMap printProductAttribute))
	(def printFinalProductMap (merge printFinalProductMap printProductMap)))

	(def printProductSortedMap (into (sorted-map) printFinalProductMap))
	
	(def printCustomer 
  		(doseq [i (sort-by first < printProductSortedMap)] 
  			(println (key i) (val i))
  		)
	)
)

; Fuction for option3

(defn option3 []
	(doseq [i (sort-by first < salesSortedMap)]
	     (def splitSortSale (list* (clojure.string/split (val i) #" ")))
	     (def getCustomerKey (nth splitSortSale 0))
	     (def getProductKey (nth splitSortSale 1))
		 (def getItemCount (nth splitSortSale 2))
		 (def getCustomerName (find mapCustomerName getCustomerKey))
	     (def getProductName (find mapProductItem getProductKey))
	     (print (key i))
	     (print ": ")
	     (print (nth getCustomerName 1 ))
	     (print ", ")
	     (print (nth getProductName 1))
	     (print ", ")
	     (print getItemCount)
	     (println)
	 )
)

; Fuction for option4

(defn option4 []
 
 (println "Please enter Customer Name")
 (let [inputName (read-line)]
 	(def myboolean false)
	(def customerName inputName)

	 (doseq [i mapCustomerName]
			  (if (= (val i) customerName) (do 
			  									(def customerKey (key i))
			  									(def myboolean true)
			  								) 
											
											"no")
	 )
	 
	 ;(if customerKey ((print "not found")) ((print "found")))
		 (cond 
		 	(= myboolean true) (do 
	 							(def floatList (list* [0.0]))
								
								 (doseq [i mapSales]

										  (def tempSalesKey ( parse-int ( nth (list* (clojure.string/split (val i) #" ")) 0)))
										  (def  intCKey  (parse-int customerKey ) )
									
										  (if (= tempSalesKey intCKey) 
												 (do
												   
												   (def prodID ( nth (list* (clojure.string/split (val i ) #" ")) 1))
												   (def itemCount (parse-int ( nth (list* (clojure.string/split (val i ) #" ")) 2)))
												   (def prodInMap (find productMap prodID))
												   (def productCost (Float. (re-find  #"\d+.\d+" (nth prodInMap 1) )))										
												   (def totalCost (format "%.2f" (with-precision 10 (* itemCount productCost))) )
												   (def floatList (cons totalCost floatList))
												 )

												 "no"
										  )
								 )

								(def totalCount (float 0.0))

								(dotimes [i (count floatList)]
									     (def totalCount (+ (Float. (nth floatList i) ) totalCount)) 
									)

								(def totalCount (format "%.2f" totalCount))

								(print inputName)
								(print ": $")
								(println totalCount)
							)
	 	(= myboolean false) (println "Customer does not exsit")
	 	)




	 	)


)


(defn option5 [] 
	(def mybool false)
	(println "Please enter product: ")
	(let [inputProduct (read-line)]

   	(def productName inputProduct)

	(doseq [i mapProductItem]
		(if (= (val i) productName) (do
										(def productKey (key i)) 
										(def mybool true)
									)
									"no")
	)

	(cond 
		(= mybool true) (do 
							(def addList (list* [0]))

							(doseq [i mapSales]

								  (def prodSalesKey ( parse-int ( nth (list* (clojure.string/split (val i ) #" ")) 1)))
								  (def  castInt (parse-int productKey ) )

								  (if (= prodSalesKey castInt) 
										 (do			   
										   (def prodID ( nth (list* (clojure.string/split (val i ) #" ")) 1))
										   (def itemCount (parse-int ( nth (list* (clojure.string/split (val i ) #" ")) 2)))
										   (def addList (cons itemCount addList))
										 )

										 "no"
								  )
							 )

							(def productCount (int 0))

							(dotimes [i (count addList)]
							     (def productCount (+ (Integer. (nth addList i) ) productCount)) 
							)

							(print productName)
							(print ": ")
							(println productCount)
							    			  
							)

			(= mybool false) (println "Product does not exist")

		)



	)

	
) 



(defn mainMenu []
(do
(println)
(println "*** Sales Menu ***
------------------
1. Display Customer Table
2. Display Product Table
3. Display Sales Table
4. Total Sales for Customer
5. Total Count for Product
6. Exit
Enter an option?
")
)
)



(defn mainProgram [s]
	(cond
    	(= s "1") 
  
    			 (do
	    				(option1)
	    				(mainMenu)
	    				(mainProgram (promptInput))
    			  )	
    	(= s "2") 
  
    			 (do
	    				(option2)
	    				(mainMenu)
	    				(mainProgram (promptInput))
    			  )		
        (= s "3") 
  
    			 (do
	    				(option3)
	    				(mainMenu)
	    				(mainProgram (promptInput))
    			  )	
        (= s "4") 
  
    			 (do
	    				(option4)
	    				(mainMenu)
	    				(mainProgram (promptInput))
    			  )

    	(= s "5") 
  
    			 (do
	    				(option5)
	    				(mainMenu)
	    				(mainProgram (promptInput))
    			  )		
    	(= s "6") 
  
    			 (do
    			 		(println "GoodBye")
    			 		(System/exit 0)		
    			  )
    	:else 	
    			(do 
					    (println "Wrong option, Choose again!!")
				 		(mainMenu)
					    (mainProgram (promptInput))
		 )							
    )
)

(mainMenu)
(let [input (read-line)]

  (cond
    (= input "1") 		
			(do 
			    (option1)
			    (mainMenu)
			    (mainProgram (promptInput))
			 )


    (= input "2") 		
   
			(do 
			    (option2)
			    (mainMenu)
			    (mainProgram (promptInput))
			 )

    (= input "3") 
    			
			(do 
			    (option3)
			    (mainMenu)
			    (mainProgram (promptInput))
			 )

    (= input "4") 
    			  
		  (do 
			    (option4)
			    (mainMenu)
			    (mainProgram (promptInput))
		   )


    (= input "5") 

		 (do 
			    (option5)
			    (mainMenu)
			    (mainProgram (promptInput))
		 )
    
    (= input "6") 

		(do 
			    (println "GoodBye")
		 		(System/exit 0)
		 )

	:else (do 
			    (println "Wrong option, Choose again!!")
		 		(mainMenu)
			    (mainProgram (promptInput))
		 )

    )
)


















