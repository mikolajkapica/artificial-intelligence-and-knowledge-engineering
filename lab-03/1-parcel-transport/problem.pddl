(define (problem delivery-mission)
    (:domain transport-logistics)

    (:objects
        warsaw krakow gdansk - city
        chopin-airport balice-airport rebiechowo-airport - airport
        port-gdynia port-gdansk port-sopot - port
        truck1 - truck
        plane1 - plane
        ship1 - ship

        package1 - package
    )

    (:init
        (at truck1 warsaw)

        (at package1 warsaw)

        (connected warsaw krakow truck1)
        (connected warsaw gdansk truck1)
        (connected gdansk krakow truck1)
        (connected chopin-airport balice-airport plane1)
        (connected chopin-airport rebiechowo-airport plane1)
        (connected rebiechowo-airport balice-airport plane1)
        (connected port-gdynia port-gdansk ship1)
        (connected port-gdynia port-sopot ship1)
        (connected port-sopot port-gdansk ship1)
        (connected krakow chopin-airport truck1)
        (connected balice-airport port-gdynia truck1)

        (= (distance warsaw krakow) 40)
        (= (distance warsaw gdansk) 10)
        (= (distance gdansk krakow) 25)
        (= (distance chopin-airport balice-airport) 50)
        (= (distance chopin-airport rebiechowo-airport) 10)
        (= (distance rebiechowo-airport balice-airport) 8)
        (= (distance port-gdynia port-gdansk) 10)
        (= (distance port-gdynia port-sopot) 32)
        (= (distance port-sopot port-gdansk) 8)
; 
        (= (fuel-cost-factor truck1) 2)
        (= (fuel-cost-factor plane1) 10)
        (= (fuel-cost-factor ship1) 5)

        (= (load-cost truck1) 5)
        (= (load-cost plane1) 10)
        (= (load-cost ship1) 8)

        (= (loaded-items truck1) 0)
        (= (max-package-capacity truck1) 2)
        (= (loaded-items plane1) 0)
        (= (max-package-capacity plane1) 3)
        (= (loaded-items ship1) 0)
        (= (max-package-capacity ship1) 4)

        (= (total-cost) 0)
    )

    (:goal
        (and
            (at package1 port-sopot)
        )
    )

    (:metric minimize
        (total-cost)
    )
)
