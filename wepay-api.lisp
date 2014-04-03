(in-package wepay)

;;; The WePay API



;;; wepay API function wepay-app

(export 'wepay-app)

(defun wepay-app ()
  "This call allows you to lookup the details of your API application."
  (let ((parameters (list)))
    (wepay-api-call "/app"
                    :parameters
                    parameters
                    client-secret
                    t
                    client-id
                    t)))

;;; wepay API function wepay-user

(export 'wepay-user)

(defun wepay-user ()
  "This call allows you to lookup the details of the user associated with the access token you are using to make the call."
  (let ((parameters (list)))
    (wepay-api-call "/user" :parameters parameters)))

;;; wepay API function wepay-user-modify

(export 'wepay-user-modify)

(defun wepay-user-modify (&key callback-uri)
  "This call allows you to add a callback_uri to the user object.
		If you add a callback_uri you will receive IPNs with the user_id each time the user revokes their access_token or is deleted."
  (let ((parameters (list)))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (wepay-api-call "/user/modify" :parameters parameters)))

;;; wepay API function wepay-user-register

(export 'wepay-user-register)

(defun wepay-user-register
       (email scope first-name last-name original-ip original-device
        &key redirect-uri callback-uri tos-acceptance-time)
  "Registers a user with your application and returns a temporary access token for that user."
  (let ((parameters
         (list (cons "email" email)
               (cons "scope" scope)
               (cons "first_name" first-name)
               (cons "last_name" last-name)
               (cons "original_ip" original-ip)
               (cons "original_device" original-device))))
    (when redirect-uri
      (push (cons "redirect_uri" redirect-uri) parameters))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (when tos-acceptance-time
      (push (cons "tos_acceptance_time" tos-acceptance-time)
            parameters))
    (wepay-api-call "/user/register"
                    :parameters
                    parameters
                    client-secret
                    t
                    client-id
                    t)))

;;; wepay API function wepay-account

(export 'wepay-account)

(defun wepay-account ()
  "This call allows you to lookup the details of a payment account on WePay. 
		The payment account must belong to the user associated with the access token used to make the call."
  (let ((parameters (list)))
    (wepay-api-call "/account" :parameters parameters account-id t)))

;;; wepay API function wepay-account-delete

(export 'wepay-account-delete)

(defun wepay-account-delete (&key reason)
  "Deletes the account specified.
		The use associated with the access token used must have permission to delete the account.
		An account may not be deleted if it has a balance or pending payments."
  (let ((parameters (list)))
    (when reason (push (cons "reason" reason) parameters))
    (wepay-api-call "/account/delete"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-account-get-update-uri

(export 'wepay-account-get-update-uri)

(defun wepay-account-get-update-uri (&key mode redirect-uri)
  "This call allows you to add or update all incomplete items for an account like KYC info, bank account, etc. It will return a URL that a user can visit to update info for his or her account."
  (let ((parameters (list)))
    (when mode (push (cons "mode" mode) parameters))
    (when redirect-uri
      (push (cons "redirect_uri" redirect-uri) parameters))
    (wepay-api-call "/account/get_update_uri"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-account-get-reserve-details

(export 'wepay-account-get-reserve-details)

(defun wepay-account-get-reserve-details (&key currency)
  "This call returns information about reserves and release schedule for a particular account."
  (let ((parameters (list)))
    (when currency (push (cons "currency" currency) parameters))
    (wepay-api-call "/account/get_reserve_details"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-checkout

(export 'wepay-checkout)

(defun wepay-checkout (checkout-id)
  "This call allows you to lookup the details of a specific checkout on WePay using the checkout_id.
		Response parameters marked \"if available\" will only show up if they have values."
  (let ((parameters (list (cons "checkout_id" checkout-id))))
    (wepay-api-call "/checkout" :parameters parameters)))

;;; wepay API function wepay-checkout-find

(export 'wepay-checkout-find)

(defun wepay-checkout-find
       (&key start limit reference-id state preapproval-id start-time
        end-time sort-order shipping-fee)
  "This call allows you to search for checkouts associated with an account.
		Returns an array of matching checkouts."
  (let ((parameters (list)))
    (when start (push (cons "start" start) parameters))
    (when limit (push (cons "limit" limit) parameters))
    (when reference-id
      (push (cons "reference_id" reference-id) parameters))
    (when state (push (cons "state" state) parameters))
    (when preapproval-id
      (push (cons "preapproval_id" preapproval-id) parameters))
    (when start-time (push (cons "start_time" start-time) parameters))
    (when end-time (push (cons "end_time" end-time) parameters))
    (when sort-order (push (cons "sort_order" sort-order) parameters))
    (when shipping-fee
      (push (cons "shipping_fee" shipping-fee) parameters))
    (wepay-api-call "/checkout/find"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-checkout-cancel

(export 'wepay-checkout-cancel)

(defun wepay-checkout-cancel (checkout-id cancel-reason)
  "Cancels the payment associated with the checkout created by the application. Checkout must be in \"authorized\" or \"reserved\" state."
  (let ((parameters
         (list (cons "checkout_id" checkout-id)
               (cons "cancel_reason" cancel-reason))))
    (wepay-api-call "/checkout/cancel" :parameters parameters)))

;;; wepay API function wepay-checkout-refund

(export 'wepay-checkout-refund)

(defun wepay-checkout-refund
       (checkout-id refund-reason &key amount app-fee
        payer-email-message payee-email-message)
  "Refunds the payment associated with the checkout created by the application. Checkout must be in \"captured\" state."
  (let ((parameters
         (list (cons "checkout_id" checkout-id)
               (cons "refund_reason" refund-reason))))
    (when amount (push (cons "amount" amount) parameters))
    (when app-fee (push (cons "app_fee" app-fee) parameters))
    (when payer-email-message
      (push (cons "payer_email_message" payer-email-message)
            parameters))
    (when payee-email-message
      (push (cons "payee_email_message" payee-email-message)
            parameters))
    (wepay-api-call "/checkout/refund" :parameters parameters)))

;;; wepay API function wepay-checkout-capture

(export 'wepay-checkout-capture)

(defun wepay-checkout-capture (checkout-id)
  "If auto_capture was set to false when the checkout was created, you will need to make this call to release funds to the account.
		Until you make this call the money will be held by WePay and if you do not capture the funds within 14 days 
		then the payment will be automatically cancelled or refunded.
		You can only make this call if the checkout is in state 'reserved'."
  (let ((parameters (list (cons "checkout_id" checkout-id))))
    (wepay-api-call "/checkout/capture" :parameters parameters)))

;;; wepay API function wepay-checkout-modify

(export 'wepay-checkout-modify)

(defun wepay-checkout-modify (checkout-id &key callback-uri)
  "This call allows you to modify the callback_uri of a checkout."
  (let ((parameters (list (cons "checkout_id" checkout-id))))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (wepay-api-call "/checkout/modify" :parameters parameters)))

;;; wepay API function wepay-preapproval

(export 'wepay-preapproval)

(defun wepay-preapproval (preapproval-id)
  "This call allows you to lookup the details of a payment preapproval on WePay."
  (let ((parameters (list (cons "preapproval_id" preapproval-id))))
    (wepay-api-call "/preapproval" :parameters parameters)))

;;; wepay API function wepay-preapproval-cancel

(export 'wepay-preapproval-cancel)

(defun wepay-preapproval-cancel (preapproval-id)
  "Allows you to cancel the preapproval.
		If cancelled the preapproval cannot be used to execute payments."
  (let ((parameters (list (cons "preapproval_id" preapproval-id))))
    (wepay-api-call "/preapproval/cancel" :parameters parameters)))

;;; wepay API function wepay-preapproval-modify

(export 'wepay-preapproval-modify)

(defun wepay-preapproval-modify (preapproval-id &key callback-uri)
  "This call allows you to modify the callback_uri on a preapproval."
  (let ((parameters (list (cons "preapproval_id" preapproval-id))))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (wepay-api-call "/preapproval/modify" :parameters parameters)))

;;; wepay API function wepay-withdrawal

(export 'wepay-withdrawal)

(defun wepay-withdrawal (withdrawal-id)
  "This call allows you to lookup the details of a withdrawal.
		A withdrawal object represents the movement of money from a WePay account to a bank account."
  (let ((parameters (list (cons "withdrawal_id" withdrawal-id))))
    (wepay-api-call "/withdrawal" :parameters parameters)))

;;; wepay API function wepay-withdrawal-find-

(export 'wepay-withdrawal-find-)

(defun wepay-withdrawal-find- (&key limit start sort-order)
  "This call allows you to find withdrawals."
  (let ((parameters (list)))
    (when limit (push (cons "limit" limit) parameters))
    (when start (push (cons "start" start) parameters))
    (when sort-order (push (cons "sort_order" sort-order) parameters))
    (wepay-api-call "/withdrawal/find/"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-withdrawal-create-

(export 'wepay-withdrawal-create-)

(defun wepay-withdrawal-create-
       (&key currency redirect-uri callback-uri fallback-uri note mode)
  "This call allows you to create a withdrawal object.
		A withdrawal object represents the movement of money from a WePay account to a bank account."
  (let ((parameters (list)))
    (when currency (push (cons "currency" currency) parameters))
    (when redirect-uri
      (push (cons "redirect_uri" redirect-uri) parameters))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (when fallback-uri
      (push (cons "fallback_uri" fallback-uri) parameters))
    (when note (push (cons "note" note) parameters))
    (when mode (push (cons "mode" mode) parameters))
    (wepay-api-call "/withdrawal/create/"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-withdrawal-modify

(export 'wepay-withdrawal-modify)

(defun wepay-withdrawal-modify (withdrawal-id &key callback-uri)
  "This call allows you to change the callback_uri on a withdrawal."
  (let ((parameters (list (cons "withdrawal_id" withdrawal-id))))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (wepay-api-call "/withdrawal/modify" :parameters parameters)))

;;; wepay API function wepay-credit-card

(export 'wepay-credit-card)

(defun wepay-credit-card (credit-card-id)
  "This call allows you to lookup the details of the a credit card that you have tokenized."
  (let ((parameters (list (cons "credit_card_id" credit-card-id))))
    (wepay-api-call "/credit_card"
                    :parameters
                    parameters
                    client-secret
                    t
                    client-id
                    t)))

;;; wepay API function wepay-credit-card-find

(export 'wepay-credit-card-find)

(defun wepay-credit-card-find
       (&key reference-id limit start sort-order)
  "Search through tokenized credit cards."
  (let ((parameters (list)))
    (when reference-id
      (push (cons "reference_id" reference-id) parameters))
    (when limit (push (cons "limit" limit) parameters))
    (when start (push (cons "start" start) parameters))
    (when sort-order (push (cons "sort_order" sort-order) parameters))
    (wepay-api-call "/credit_card/find"
                    :parameters
                    parameters
                    client-secret
                    t
                    client-id
                    t)))

;;; wepay API function wepay-credit-card-delete

(export 'wepay-credit-card-delete)

(defun wepay-credit-card-delete (credit-card-id)
  "Delete the credit card when you don't need it anymore. Note that you won't be able to use this card to make payments any more."
  (let ((parameters (list (cons "credit_card_id" credit-card-id))))
    (wepay-api-call "/credit_card/delete"
                    :parameters
                    parameters
                    client-secret
                    t
                    client-id
                    t)))

;;; wepay API function wepay-subscription

(export 'wepay-subscription)

(defun wepay-subscription (subscription-id)
  "This call allows you to lookup the details of a specific subscription on WePay using the subscription_id."
  (let ((parameters (list (cons "subscription_id" subscription-id))))
    (wepay-api-call "/subscription" :parameters parameters)))

;;; wepay API function wepay-subscription-find

(export 'wepay-subscription-find)

(defun wepay-subscription-find
       (subscription-plan-id &key start limit start-time end-time state
        reference-id)
  "This call allows you to search for subscriptions that match the search parameters."
  (let ((parameters
         (list (cons "subscription_plan_id" subscription-plan-id))))
    (when start (push (cons "start" start) parameters))
    (when limit (push (cons "limit" limit) parameters))
    (when start-time (push (cons "start_time" start-time) parameters))
    (when end-time (push (cons "end_time" end-time) parameters))
    (when state (push (cons "state" state) parameters))
    (when reference-id
      (push (cons "reference_id" reference-id) parameters))
    (wepay-api-call "/subscription/find" :parameters parameters)))

;;; wepay API function wepay-subscription-create

(export 'wepay-subscription-create)

(defun wepay-subscription-create
       (subscription-plan-id &key redirect-uri callback-uri
        payment-method-id payment-method-type mode quantity
        reference-id prefill-info)
  "Creates a subscription for a subscription plan."
  (let ((parameters
         (list (cons "subscription_plan_id" subscription-plan-id))))
    (when redirect-uri
      (push (cons "redirect_uri" redirect-uri) parameters))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (when payment-method-id
      (push (cons "payment_method_id" payment-method-id) parameters))
    (when payment-method-type
      (push (cons "payment_method_type" payment-method-type)
            parameters))
    (when mode (push (cons "mode" mode) parameters))
    (when quantity (push (cons "quantity" quantity) parameters))
    (when reference-id
      (push (cons "reference_id" reference-id) parameters))
    (when prefill-info
      (push (cons "prefill_info" prefill-info) parameters))
    (wepay-api-call "/subscription/create" :parameters parameters)))

;;; wepay API function wepay-subscription-cancel

(export 'wepay-subscription-cancel)

(defun wepay-subscription-cancel (subscription-id &key reason)
  "Cancels a subscription."
  (let ((parameters (list (cons "subscription_id" subscription-id))))
    (when reason (push (cons "reason" reason) parameters))
    (wepay-api-call "/subscription/cancel" :parameters parameters)))

;;; wepay API function wepay-subscription-plan

(export 'wepay-subscription-plan)

(defun wepay-subscription-plan (subscription-plan-id)
  "This call allows you to lookup the details of a specific subscription plan on WePay using the subscription_plan_id."
  (let ((parameters
         (list (cons "subscription_plan_id" subscription-plan-id))))
    (wepay-api-call "/subscription_plan" :parameters parameters)))

;;; wepay API function wepay-subscription-plan-find

(export 'wepay-subscription-plan-find)

(defun wepay-subscription-plan-find
       (&key start limit state reference-id)
  "This call allows you to search for subscription plans associated with an account. If no account_id is passed, then it will look for subscription plans on an app level basis.
		Returns an array of matching subscription plans."
  (let ((parameters (list)))
    (when start (push (cons "start" start) parameters))
    (when limit (push (cons "limit" limit) parameters))
    (when state (push (cons "state" state) parameters))
    (when reference-id
      (push (cons "reference_id" reference-id) parameters))
    (wepay-api-call "/subscription_plan/find"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-subscription-plan-create

(export 'wepay-subscription-plan-create)

(defun wepay-subscription-plan-create
       (name short-description amount period &key currency app-fee
        callback-uri trial-length setup-fee reference-id)
  "Creates a subscription_plan for an account."
  (let ((parameters
         (list (cons "name" name)
               (cons "short_description" short-description)
               (cons "amount" amount)
               (cons "period" period))))
    (when currency (push (cons "currency" currency) parameters))
    (when app-fee (push (cons "app_fee" app-fee) parameters))
    (when callback-uri
      (push (cons "callback_uri" callback-uri) parameters))
    (when trial-length
      (push (cons "trial_length" trial-length) parameters))
    (when setup-fee (push (cons "setup_fee" setup-fee) parameters))
    (when reference-id
      (push (cons "reference_id" reference-id) parameters))
    (wepay-api-call "/subscription_plan/create"
                    :parameters
                    parameters
                    account-id
                    t)))

;;; wepay API function wepay-subscription-plan-delete

(export 'wepay-subscription-plan-delete)

(defun wepay-subscription-plan-delete
       (subscription-plan-id &key reason)
  "Deletes the subscription plan. Existing subscriptions to the plan will still be active, but there will be no new subscriptions to the plan."
  (let ((parameters
         (list (cons "subscription_plan_id" subscription-plan-id))))
    (when reason (push (cons "reason" reason) parameters))
    (wepay-api-call "/subscription_plan/delete"
                    :parameters
                    parameters)))

;;; wepay API function wepay-subscription-charge

(export 'wepay-subscription-charge)

(defun wepay-subscription-charge (subscription-charge-id)
  "This call allows you to lookup the details of a specific subscription charge on WePay using a subscription_charge_id."
  (let ((parameters
         (list (cons "subscription_charge_id"
                     subscription-charge-id))))
    (wepay-api-call "/subscription_charge" :parameters parameters)))

;;; wepay API function wepay-subscription-charge-find

(export 'wepay-subscription-charge-find)

(defun wepay-subscription-charge-find
       (subscription-id &key start limit start-time end-time type
        amount state)
  "This call allows you to search for subscription charges (for a specific subscription) that match the search parameters."
  (let ((parameters (list (cons "subscription_id" subscription-id))))
    (when start (push (cons "start" start) parameters))
    (when limit (push (cons "limit" limit) parameters))
    (when start-time (push (cons "start_time" start-time) parameters))
    (when end-time (push (cons "end_time" end-time) parameters))
    (when type (push (cons "type" type) parameters))
    (when amount (push (cons "amount" amount) parameters))
    (when state (push (cons "state" state) parameters))
    (wepay-api-call "/subscription_charge/find"
                    :parameters
                    parameters)))

;;; wepay API function wepay-subscription-charge-refund

(export 'wepay-subscription-charge-refund)

(defun wepay-subscription-charge-refund
       (subscription-charge-id &key refund-reason)
  "Refunds a subscription charge."
  (let ((parameters
         (list (cons "subscription_charge_id"
                     subscription-charge-id))))
    (when refund-reason
      (push (cons "refund_reason" refund-reason) parameters))
    (wepay-api-call "/subscription_charge/refund"
                    :parameters
                    parameters)))