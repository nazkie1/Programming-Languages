#lang scheme

(define customers '(
                   ("John Smith" 35 "New York")
                   ("Alice Johnson" 28 "Los Angeles")
                   ("Michael Brown" 45 "Miami")
                   ("Emily Davis" 32 "Houston")
                   ("Robert Wilson" 40 "Miami")
                   ("Sophia Martinez" 30 "New York")
                   ("William Taylor" 38 "Houston")
                   ("Emma White" 25 "Los Angeles")
                   ("James Harris" 32 "Houston")
                   ("Olivia Clark" 29 "Los Angeles")))

(define items '(
              ("Apples" "Fruits"  2.30)
              ("Coffee" "Beverages" 3.50)
              ("Bread" "Bakery" 2.00)
              ("Milk" "Dairy" 3.50)
              ("Bananas" "Fruits" 1.75)
              ("Eggs" "Dairy" 4.75)
              ("Orange-Juice" "Beverages" 3.25)
              ("Tea" "Beverages" 2.75)
              ("Fish" "Seafood" 12.50)
              ("Broccoli" "Vegetables" 1.80)
              ("Orange" "Fruits" 1.25)
              ("Chicken" "Meat" 7.00)
              ("Lettuce" "Vegetables" 1.20)
              ("Pasta" "Pantry" 3.75)
              ("Salmon" "Seafood" 9.50)
              ("Yogurt" "Dairy" 2.75)
              ("Bacon" "Meat" 6.25)
              ("Cheese" "Dairy" 5.50)
              ("Beef" "Meat" 8.00)
              ("Potatoes" "Vegetables" 2.50)
              ("Chicken-Soup" "Canned-Goods" 3.50)
              ("Rice" "Grains" 2.25)
              ("Carrots" "Vegetables" 1.10)
              ("Spinach" "Vegetables" 1.60)
              ("Tomatoes" "Vegetables" 1.50)
              ("Apple-Juice" "Beverages" 3.40)
              ("Onions" "Vegetables" 1.20)))

(define purchases'(
                ("John Smith"
                   (("Apples" "Coffee" "Bread") ("15.03.2024"))
                   (("Milk" "Bananas") ("22.03.2024"))
                   (("Eggs" "Orange-Juice") ("29.03.2024"))
                   (("Tea" "Fish" "Broccoli" "Orange") ("5.04.2024"))
                   (("Chicken" "Lettuce" "Pasta" "Salmon") ("12.04.2024")))

                ("Alice Johnson"
                    (("Milk" "Bananas") ("20.03.2024")))
                   
                ("Michael Brown"
                    (("Orange-Juice" "Yogurt") ("24.03.2024"))
                    (("Bacon") ("28.03.2024"))
                    (("Coffee" "Bread" "Apples") ("2.04.2024"))
                    (("Milk" "Bananas" "Eggs") ("5.04.2024"))
                    (("Cheese" "Beef" "Potatoes" "Chicken-Soup") ("10.04.2024")))                
                      
                ("Emily Davis"
                    (("Chicken" "Lettuce") ("24.03.2024"))
                    (("Pasta" "Salmon" "Rice" "Potatoes") ("28.03.2024"))
                    (("Carrots" "Spinach") ("1.04.2024")))
                      
                ("Robert Wilson"
                    (("Salmon" "Rice") ("21.03.2024"))
                    (("Potatoes" "Chicken" "Lettuce" "Pasta")( "25.03.2024"))
                    (("Milk" "Bananas" "Eggs" "Orange-Juice") ("29.03.2024"))
                    (("Bacon") ("2.04.2024"))
                    (("Fish" "Broccoli") ("6.04.2024")))
            
               ("Sophia Martinez"
                    (("Carrots" "Spinach") ("26.03.2024"))
                    (("Tea" "Fish" "Broccoli" "Orange") ("30.03.2024")))
               
               ("William Taylor"
                    (("Beef" "Potatoes") ("19.03.2024"))
                    (("Chicken-Soup" "Tomatoes" "Apple-Juice" "Bread") ("23.03.2024")))

               ("Emma White"
                    (("Tomatoes" "Chicken-Soup") ("23.03.2024"))
                    (("Milk" "Salmon" "Rice" "Potatoes") ("27.03.2024"))
                    (("Chicken" "Lettuce" "Pasta" "Salmon") ("31.03.2024")))

               ("James Harris"
                    (("Onions" "Apple-Juice") ("25.03.2024"))
                    (("Cheese" "Beef" "Potatoes" "Chicken-Soup") ("29.03.2024")))

               ("Olivia Clark"
                    (("Fish" "Broccoli") ("25.03.2024"))
                    (("Orange" "Chicken" "Lettuce" "Pasta") ("29.03.2024")))))
                

;FIRST FUNCTION
(display "All items:")
(define (retrieve-all-items items)
  (if (null? items) '()
     (cons (caar items) (retrieve-all-items (cdr items)))))   
(display (retrieve-all-items items))                              

(newline)
(newline)


;2 helper
(define (retrieve items index)
  (define (unique_items items result)
    (if (null? items)
        result
    (let ((item (car items)))
       (if (not (member (list-ref item index) result))
         (unique_items (cdr items) (cons (list-ref item index) result)) 
            (unique_items (cdr items) result)))))
  (unique_items items '()))

;SECOND FUNCTION
(display "All categories:")
(define (retrieve_all_categories items) (retrieve items 1))
(display(retrieve_all_categories items))

(newline)
(newline)


;ÜÇÜNCÜ FONKSİYON
;reads the input and returns a list of elements in that category

(define (retrieve-items-by-category items)
  (display "Enter a category: ")
  (newline)
  (define category (read-line))
  (items-in-category items '() category)) 

(define (items-in-category items items-list category)
  (if (null? items)
      (reverse items-list) ; Reverse the items-list before returning
      (let ((item (car items)))
        (if (string=? category (cadr item))
            (items-in-category (cdr items) (cons (car item) items-list) category)
            (items-in-category (cdr items) items-list category)))))

(display (retrieve-items-by-category items))

(newline)
(newline)


; 4 5 helper
;gets customer or item name to retrieve info by name 
(define (get-customer-or-item name mylist)
  (if (null? mylist)
      "Not found"
      (let ((myelement (car mylist)))
        (if (equal? (car myelement) name)
            myelement
            (get-customer-or-item name (cdr mylist))))))

;FORTH FUNCTION
;gets customer information
(define (get-customer-information name)
  (get-customer-or-item name customers))
(display "Customer informations:")
(newline)
(display (get-customer-information "John Smith"))
(newline)
(display (get-customer-information "Emma White"))
(newline)
(display (get-customer-information "Naz Ödenir"))

(newline)
(newline)
 

;FIFTH FUNCTION

;gets item information
(display "Item Information:")
(newline)
(define (get-item-information name)
  (get-customer-or-item name items))

(display (get-item-information "Apples"))
(newline)
(display (get-item-information "Salmon"))
(newline)
(display (get-item-information "Pears"))

(newline)
(newline)

;SIXTH FUNCTION
;finds the expensive
(define (find-expensive items max-price max-item)
  (if (null? items)
      max-item
      (let ((current-price (caddar items)))
        (if (> current-price max-price)
            (find-expensive (cdr items) current-price (car items))
            (find-expensive (cdr items) max-price max-item)))))

(define (most-expensive-item items)
  (if (null? items)
      "Item list empty."
      (find-expensive items (caddr (car items)) (car items))))
(display "Most expensive item:")
(display (most-expensive-item items))

(newline)
(newline)

;SEVENTH FUNCTION

;finds the cheap
(define(find-cheap items min-price min-item)
  (if (null? items)
      min-item
      (let((current-price (caddar items)))
        (if (< current-price min-price) (find-cheap (cdr items) current-price (car items))
            (find-cheap (cdr items) min-price min-item)))))

(define (cheapest-item items)
  (if (null? items) "Item list empty."
      (find-cheap items (caddar items) (car items))))

(display "Cheapest item:")
(display(cheapest-item items))

(newline)
(newline)



;8 helper
;finds the price for a item from items list
(define (find-price item lst)
    (cond ((null? lst) #f)
          ((string=? (caar lst) item)(caddar lst))
          (else (find-price item (cdr lst)))))

;gets item price for a given item 
(define (get-item-price item)
  (find-price item items)) 

;(display (get-item-price "Apples"))

(newline)
(newline)

;NINTH FUNCTION

;retrieves all items bought for given customer as a list
(define (items-bought-by-specific-customer name)
  (define (get-items purchases)
    (cond ((null? purchases) '())
          ((string=? (caar purchases) name)
           (apply append (map car (cdar purchases))))
          (else (get-items (cdr purchases))))) 
  (get-items purchases))

(display "Items bought by customer:")
(display(items-bought-by-specific-customer "William Taylor"))

(newline)
(newline)


;EIGHTH FUNCTION

;returns the total price for the list obtained from ninth function

(define (calculate-customer-bill list) 
 (let loop ((total 0) (remaining-items list)) 
     (cond
      ((null? remaining-items) total)
      (else
       ( let((current-item (car remaining-items) ))
          (loop (+ total (get-item-price current-item)) (cdr remaining-items)))))))

(display "Bill for customer: ")
(display (calculate-customer-bill (items-bought-by-specific-customer "John Smith")))

(newline)
(newline)

;TENTH FUNCTION
;gets all customer names as a list
(define (get-all-customer-names)
  (map car customers))

;loops through all customer names and adds up all their bills
(define (total-cost-of-transactions)
   (let loop ((total 0) (customer-names (get-all-customer-names) )) 
     (cond
      ((null? customer-names) total)
      (else
       ( let((current-name (car customer-names) ))
          (loop (+ total (calculate-customer-bill (items-bought-by-specific-customer current-name))) (cdr customer-names)))))))


(display "Total cost of transactions:")
(display(total-cost-of-transactions))

(newline)
(newline)

;ELEVENTH FUNCTION

;gets all purchases with dates in a list
(define (get-all-purchases-with-dates)
  (map cdr purchases))

(define (all-purchases-with-dates table )
   (let loop ((purchases '()) (mytable table)) 
     (cond
      ((null? mytable) purchases)
      (else
       ( let((current-name (car mytable) ))
          (loop (append purchases current-name) (cdr mytable)))))))


(define (items-purchased-on-specific-date date)
  (filter (lambda (list) (string=? date (car (cadr list)))) (all-purchases-with-dates (get-all-purchases-with-dates)) ))

(display ";Purchased on given date:")
(items-purchased-on-specific-date "25.03.2024")

(newline)
(newline)

;TWELFTH FUNCTION


;THIRTEENTH

;helper : returns every item bought by all customers
(define (get-all-items-of-all-customers customers)
  (define (get-all-items-of-customer customer-name)
    (items-bought-by-specific-customer customer-name))
  
  (define (get-all-items-helper names)
    (if (null? names)
        '()
        (append (get-all-items-of-customer (car names))
                (get-all-items-helper (cdr names)))))
  
  (get-all-items-helper (get-all-customer-names)))

;;(display (get-all-items-of-all-customers customers))


;helper: returns categories from the list obtained from previous helper
(define (find-categories customer-items items)
  (define (lookup-category item)
    (cadr (assoc item items)))
  
  (define (helper items)
    (if (null? items)
        '()
        (cons (lookup-category (car items))
              (helper (cdr items)))))
  
  (helper customer-items))

;;(display (find-categories (get-all-items-of-all-customers customers) items))

;counts category occurances for the list 
(define (most-popular-category categories)
  (define (count-occurrences item lst)
    (length (filter (lambda (x) (equal? x item)) lst)))
  
  (let ((category-counts
         (map (lambda (x) (cons x (count-occurrences x categories))) categories)))
    (caar (sort category-counts (lambda (x y) (> (cdr x) (cdr y)))))))

(display "Most popular category is:")
(display (most-popular-category (find-categories (get-all-items-of-all-customers customers) items)))

(newline)
(newline)

;ON DÖRDÜNCÜ FONKSİYON

;returns a list of customers with their ages

(define (get-customers-with-age customers)
  (map (lambda (customer) (list (car customer) (cadr customer))) customers))

;(display (get-customers-with-age customers))

;helps get customer names in a list acording to age range
(define (get-customers-names-in-age-range customers min-age max-age)
  (map car (filter (lambda (customer) (and (>= (cadr customer) min-age) (<= (cadr customer) max-age))) customers)))

;(display (car (get-customers-names-in-age-range customers 30 40)))

;gets prchased items for age range
(define (get-purchased-items-in-age-range customers purchases min-age max-age)
  (define customers-in-range (get-customers-names-in-age-range customers min-age max-age))
  
  ;gathers the bought items for that age range in a list
  (define purchased-items '())
  (for-each (lambda (customer) 
              (set! purchased-items (append purchased-items (items-bought-by-specific-customer customer))))
            customers-in-range)
  
  ;removes duplicates
  (remove-duplicates purchased-items))

(display "Items purchased by given age group:")
(display (get-purchased-items-in-age-range customers purchases 20 30))

(newline)
(newline)


;ON BEŞİNCİ FONKSİYON

;total bill of purchases for each location
(define (average-spending-per-person-by-location customers)
  (define (total-cost-by-location location)
    (define location-customers
      (filter (lambda (customer) (string=? (caddr customer) location)) customers))
    (define total-cost (apply + (map (lambda (customer) (calculate-customer-bill (items-bought-by-specific-customer (car customer)))) location-customers)))
    (define total-customers (length location-customers))
    (list location (/ total-cost total-customers)))
  
  ;finds average spending for locations per person
  (map total-cost-by-location (remove-duplicates (map caddr customers))))

(display "Average spending per transaction by location: ")
(newline)
(newline)

;displays
(define (display-average-spending-per-transaction-by-location customers)
  (define averages (average-spending-per-person-by-location customers))
  
  (define (display-location-average location average)
    (display location)
    (display ": ")
    (display average)
    (newline))
  
  (display-location-average "New York" (/ 40.9 7))
  (display-location-average "Los Angeles" (/ 25.650000000000002 6))
  (display-location-average "Miami" (/ 54.775 10))
  (display-location-average "Houston" (/ 24.633333333333336 7)))

(display-average-spending-per-transaction-by-location customers)














