;; Helix Quality - Supply Chain Quality Assurance
;; A blockchain-based quality prediction and compliance system

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-quality (err u103))
(define-constant err-batch-isolated (err u104))

;; Data Variables
(define-data-var min-quality-threshold uint u70)
(define-data-var risk-alert-threshold uint u50)

;; Data Maps
;; Supply chain participants
(define-map participants
    principal
    {
        name: (string-ascii 50),
        participant-type: (string-ascii 20),
        authorized: bool,
        quality-score: uint
    }
)

;; Batch tracking
(define-map batches
    {batch-id: (string-ascii 50)}
    {
        product-name: (string-ascii 100),
        manufacturer: principal,
        quality-score: uint,
        risk-level: (string-ascii 10),
        status: (string-ascii 20),
        timestamp: uint,
        environmental-data: (string-ascii 200),
        isolated: bool
    }
)

;; Quality predictions
(define-map quality-predictions
    {batch-id: (string-ascii 50)}
    {
        predicted-score: uint,
        confidence: uint,
        factors: (string-ascii 200),
        prediction-timestamp: uint
    }
)

;; Compliance records
(define-map compliance-proofs
    {batch-id: (string-ascii 50), checkpoint: (string-ascii 50)}
    {
        verified: bool,
        verifier: principal,
        timestamp: uint,
        compliance-hash: (buff 32)
    }
)

;; Quality events log
(define-map quality-events
    {event-id: uint}
    {
        batch-id: (string-ascii 50),
        event-type: (string-ascii 30),
        severity: (string-ascii 10),
        description: (string-ascii 200),
        timestamp: uint,
        actor: principal
    }
)

(define-data-var event-counter uint u0)

;; Read-only functions
(define-read-only (get-participant (participant principal))
    (map-get? participants participant)
)

(define-read-only (get-batch (batch-id (string-ascii 50)))
    (map-get? batches {batch-id: batch-id})
)

(define-read-only (get-quality-prediction (batch-id (string-ascii 50)))
    (map-get? quality-predictions {batch-id: batch-id})
)

(define-read-only (get-compliance-proof (batch-id (string-ascii 50)) (checkpoint (string-ascii 50)))
    (map-get? compliance-proofs {batch-id: batch-id, checkpoint: checkpoint})
)

(define-read-only (get-quality-threshold)
    (var-get min-quality-threshold)
)

(define-read-only (get-risk-threshold)
    (var-get risk-alert-threshold)
)

(define-read-only (is-batch-compliant (batch-id (string-ascii 50)))
    (match (map-get? batches {batch-id: batch-id})
        batch (ok (>= (get quality-score batch) (var-get min-quality-threshold)))
        (err err-not-found)
    )
)

;; Public functions
;; Register a supply chain participant
(define-public (register-participant (participant principal) (name (string-ascii 50)) (participant-type (string-ascii 20)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set participants participant
            {
                name: name,
                participant-type: participant-type,
                authorized: true,
                quality-score: u100
            }
        ))
    )
)

;; Create a new batch with quality tracking
(define-public (create-batch 
    (batch-id (string-ascii 50))
    (product-name (string-ascii 100))
    (initial-quality uint)
    (environmental-data (string-ascii 200))
)
    (let
        (
            (participant-info (unwrap! (map-get? participants tx-sender) err-unauthorized))
        )
        (asserts! (get authorized participant-info) err-unauthorized)
        (asserts! (and (>= initial-quality u0) (<= initial-quality u100)) err-invalid-quality)
        (ok (map-set batches {batch-id: batch-id}
            {
                product-name: product-name,
                manufacturer: tx-sender,
                quality-score: initial-quality,
                risk-level: (calculate-risk-level initial-quality),
                status: "active",
                timestamp: block-height,
                environmental-data: environmental-data,
                isolated: false
            }
        ))
    )
)

;; Update quality score with predictive analytics
(define-public (update-quality-score 
    (batch-id (string-ascii 50))
    (new-quality-score uint)
    (predicted-score uint)
    (confidence uint)
    (factors (string-ascii 200))
)
    (let
        (
            (batch-info (unwrap! (map-get? batches {batch-id: batch-id}) err-not-found))
            (participant-info (unwrap! (map-get? participants tx-sender) err-unauthorized))
        )
        (asserts! (get authorized participant-info) err-unauthorized)
        (asserts! (not (get isolated batch-info)) err-batch-isolated)
        (asserts! (and (>= new-quality-score u0) (<= new-quality-score u100)) err-invalid-quality)
        
        ;; Store prediction
        (map-set quality-predictions {batch-id: batch-id}
            {
                predicted-score: predicted-score,
                confidence: confidence,
                factors: factors,
                prediction-timestamp: block-height
            }
        )
        
        ;; Update batch quality
        (map-set batches {batch-id: batch-id}
            (merge batch-info {
                quality-score: new-quality-score,
                risk-level: (calculate-risk-level new-quality-score)
            })
        )
        
        ;; Auto-isolate if below threshold
        (if (< new-quality-score (var-get risk-alert-threshold))
            (isolate-batch batch-id)
            (log-quality-event batch-id "quality-update" (calculate-risk-level new-quality-score) "Quality score updated with prediction")
        )
    )
)

;; Record compliance proof
(define-public (record-compliance 
    (batch-id (string-ascii 50))
    (checkpoint (string-ascii 50))
    (compliance-hash (buff 32))
)
    (let
        (
            (batch-info (unwrap! (map-get? batches {batch-id: batch-id}) err-not-found))
            (participant-info (unwrap! (map-get? participants tx-sender) err-unauthorized))
        )
        (asserts! (get authorized participant-info) err-unauthorized)
        (ok (map-set compliance-proofs {batch-id: batch-id, checkpoint: checkpoint}
            {
                verified: true,
                verifier: tx-sender,
                timestamp: block-height,
                compliance-hash: compliance-hash
            }
        ))
    )
)

;; Autonomous batch isolation for quality issues
(define-public (isolate-batch (batch-id (string-ascii 50)))
    (let
        (
            (batch-info (unwrap! (map-get? batches {batch-id: batch-id}) err-not-found))
        )
        (begin
            (map-set batches {batch-id: batch-id}
                (merge batch-info {
                    status: "isolated",
                    isolated: true
                })
            )
            (log-quality-event batch-id "batch-isolated" "high" "Batch automatically isolated due to quality concerns")
        )
    )
)

;; Release isolated batch after remediation
(define-public (release-batch (batch-id (string-ascii 50)))
    (let
        (
            (batch-info (unwrap! (map-get? batches {batch-id: batch-id}) err-not-found))
        )
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-owner-only)
            (asserts! (get isolated batch-info) err-not-found)
            (map-set batches {batch-id: batch-id}
                (merge batch-info {
                    status: "active",
                    isolated: false
                })
            )
            (log-quality-event batch-id "batch-released" "low" "Batch released after remediation")
        )
    )
)

;; Adaptive threshold adjustment
(define-public (adjust-quality-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (and (>= new-threshold u0) (<= new-threshold u100)) err-invalid-quality)
        (var-set min-quality-threshold new-threshold)
        (ok true)
    )
)

(define-public (adjust-risk-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (and (>= new-threshold u0) (<= new-threshold u100)) err-invalid-quality)
        (var-set risk-alert-threshold new-threshold)
        (ok true)
    )
)

;; Private functions
(define-private (calculate-risk-level (quality-score uint))
    (if (>= quality-score u80)
        "low"
        (if (>= quality-score u50)
            "medium"
            "high"
        )
    )
)

(define-private (log-quality-event 
    (batch-id (string-ascii 50))
    (event-type (string-ascii 30))
    (severity (string-ascii 10))
    (description (string-ascii 200))
)
    (let
        (
            (event-id (var-get event-counter))
        )
        (var-set event-counter (+ event-id u1))
        (ok (map-set quality-events {event-id: event-id}
            {
                batch-id: batch-id,
                event-type: event-type,
                severity: severity,
                description: description,
                timestamp: block-height,
                actor: tx-sender
            }
        ))
    )
)