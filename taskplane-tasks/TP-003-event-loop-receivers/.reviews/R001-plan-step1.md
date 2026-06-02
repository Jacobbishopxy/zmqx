## Plan Review: Step 1: Receiver design and poll integration

### Verdict: REVISE

### Summary
The plan has the right overall direction for receiver registration, context validation, and using `Zmqx.Core.Poll` instead of raw polling. However, this checkpoint is meant to confirm receiver delivery and public `recv` semantics before implementation, and the current note does not yet settle two required behaviors.

### Issues Found
1. **[Severity: important]** — `STATUS.md:109` says receivers will have “mailbox or callback modes,” but `PROMPT.md:63` specifically requires `ReceiverMode` to support **bounded** mailbox delivery. Revise the plan to state where the mailbox bound lives in the public/design shape (for example, a capacity on the mailbox mode or receiver registration) and that the implementation will use a bounded mailbox primitive; overflow behavior can still be finalized in Step 2.
2. **[Severity: important]** — `STATUS.md:109` only says to keep the `recv` helper in `Zmqx.EventLoop`, but `PROMPT.md:66` requires concrete public semantics: timeout returns `Right Nothing`, while missing, stopped, and non-mailbox endpoints return `Left Error`. Revise the plan to record those outcomes explicitly so Step 2/3 implementation and tests target the same behavior.

### Missing Items
- Explicit confirmation of bounded mailbox API/design and `recv` timeout/error semantics, as described above.

### Suggestions
- For the existential receiver set, carry both `CanPoll 'PollIn a` and multipart receive capability in the receiver wrapper so the same registered socket can be passed to `pollIn`/`pollInAlso`, checked via `Ready`, and read via `receives_`.
