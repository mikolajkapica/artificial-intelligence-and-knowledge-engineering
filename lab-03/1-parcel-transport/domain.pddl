(define (domain transport-logistics)
    (:requirements :strips :typing :negative-preconditions :conditional-effects :action-costs :durative-actions)

    (:types
        package - object
        vehicle - object
        location - object
        truck plane ship - vehicle
        city airport port - location
    )

    (:predicates
        (at ?obj - object ?loc - location)
        (in ?pkg - package ?veh - vehicle)
        (connected ?from - location ?to - location ?transport - vehicle)
    )

    (:functions
        (fuel-cost-factor ?veh - vehicle)
        (distance ?from - location ?to - location)
        (loaded-items ?veh - vehicle)
        (max-package-capacity ?veh - vehicle)
        (total-cost)
        (load-cost ?veh - vehicle)
    )

    (:durative-action drive
        :parameters (?truck - truck ?from - city ?to - city)
        :duration (= ?duration (* 2 (distance ?from ?to)))
        :condition(and
            (at start (at ?truck ?from))
            (at start (connected ?from ?to ?truck))
        )
        :effect (and
            (at start (not (at ?truck ?from)))
            (at end (at ?truck ?to))
            (at end (increase
                    (total-cost)
                    (* (fuel-cost-factor ?truck) (distance ?from ?to))))
        )
    )

    (:durative-action fly
        :parameters (?plane - plane ?from - airport ?to - airport)
        :duration (= ?duration (distance ?from ?to))
        :condition (and
            (at start (at ?plane ?from))
            (at start (connected ?from ?to ?plane))
        )
        :effect (and
            (at start (not (at ?plane ?from)))
            (at end (at ?plane ?to))
            (at end (increase
                    (total-cost)
                    (* (fuel-cost-factor ?plane) (distance ?from ?to))))
        )
    )

    (:durative-action sail
        :parameters (?ship - ship ?from - port ?to - port)
        :duration (= ?duration (* 3 (distance ?from ?to)))
        :condition (and
            (at start (at ?ship ?from))
            (at start (connected ?from ?to ?ship))
        )
        :effect (and
            (at start (not (at ?ship ?from)))
            (at end (at ?ship ?to))
            (at end (increase
                    (total-cost)
                    (* (fuel-cost-factor ?ship) (distance ?from ?to))))
        )
    )

    (:action load
        :parameters (?pkg - package ?veh - vehicle ?loc - location)
        :precondition (and
            (at ?pkg ?loc)
            (at ?veh ?loc)
            (< (loaded-items ?veh) (max-package-capacity ?veh))
        )
        :effect (and
            (in ?pkg ?veh)
            (not (at ?pkg ?loc))
            (increase (loaded-items ?veh) 1)
            (increase (total-cost) (load-cost ?veh))
        )
    )

    (:action unload
        :parameters (?pkg - package ?veh - vehicle ?loc - location)
        :precondition (and
            (in ?pkg ?veh)
            (at ?veh ?loc)
        )
        :effect (and
            (at ?pkg ?loc)
            (not (in ?pkg ?veh))
            (decrease (loaded-items ?veh) 1)
        )
    )
)
