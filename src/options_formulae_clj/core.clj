(ns options-formulae-clj.core
  (:import [org.apache.commons.math3.distribution NormalDistribution]))


;; ;; https://www.optiontradingtips.com/pricing/black-and-scholes.html
;; (defn dOne [underlying-price
;;             exercise-price
;;             risk-free-rate
;;             dividend-yield
;;             historical-volatility
;;             dte
;;             ]
;;   ;; =(LN(underlying_price/exercise_price)+(risk_free_rate-dividend_yield+0.5*historical_volatility^2)*dte)/(historical_volatility*SQRT(dte))
;;   (->
;;    (->
;;     (Math/log (/ underlying-price exercise-price))
;;     (+ (-> (- risk-free-rate dividend-yield)
;;            (+ (* 0.5 (Math/pow historical-volatility 2)))
;;            (* dte))))
;;    (/ (* historical-volatility (Math/sqrt dte)))))

(def norm-dist (NormalDistribution.))



(defn get-d1 [underlying-price strike-price time-to-expiry-in-yrs risk-free-rate volatility]
  #break
  (->
   (-> (Math/log (/ underlying-price strike-price))
       (+ (->
           (-> risk-free-rate
               (+ (-> volatility
                      (* volatility)
                      (/ 2.0))))
           (* time-to-expiry-in-yrs))))
   (/ (* volatility (Math/sqrt time-to-expiry-in-yrs)))))


(defn bs-price [call-or-put underlying-price strike-price time-to-expiry-in-yrs risk-free-rate volatility]
  (let [dividend-yield 0.0
        d1 (get-d1 underlying-price strike-price time-to-expiry-in-yrs risk-free-rate volatility)
        d2 (-> d1
               (- (* volatility (Math/sqrt time-to-expiry-in-yrs))))]
    (case call-or-put
      :call (->
             (-> underlying-price
                 (* (Math/exp (* -1 dividend-yield time-to-expiry-in-yrs)))
                 (* (.cumulativeProbability norm-dist d1)))
             (- (-> strike-price
                    (* (Math/exp (* -1 risk-free-rate time-to-expiry-in-yrs)))
                    (* (.cumulativeProbability norm-dist d2)))))
      :put (->
            (-> strike-price
                (* (Math/exp (* -1 risk-free-rate time-to-expiry-in-yrs)))
                (* (.cumulativeProbability norm-dist (* -1 d2))))
            (- (-> underlying-price
                   (* (Math/exp (* -1 dividend-yield time-to-expiry-in-yrs)))
                   (* (.cumulativeProbability norm-dist (* -1 d1))))))
      (throw (Exception. "invalid value for call-or-put")))))

(defn bs-vega [call-or-put underlying-price strike-price time-to-expiry-in-yrs risk-free-rate volatility]
  (let [d1 (get-d1 underlying-price strike-price time-to-expiry-in-yrs risk-free-rate volatility)]
    (* underlying-price
       (Math/sqrt time-to-expiry-in-yrs)
       (.density norm-dist d1))))

(defn find-volatility [target-value call-or-put underlying-price strike-price time-to-expiry-in-yrs risk-free-rate]
  "target-value is current option price"
  (let [precision 0.00001
        sigma 0.5]
    (loop [result 0.5
           iter-count 0]
      (let [price (bs-price call-or-put underlying-price strike-price time-to-expiry-in-yrs risk-free-rate result)
            vega (bs-vega call-or-put underlying-price strike-price time-to-expiry-in-yrs risk-free-rate result)
            diff (- target-value price)]
       (if (or (< (Math/abs diff) precision) (>= iter-count 100))
         result
         (recur (+ result (/ diff vega)) (inc iter-count)))))
    ))

(defn test-find-volatility []
  (find-volatility 17.5 :call 586.08 585 (/ 40 365) 0.0002))
