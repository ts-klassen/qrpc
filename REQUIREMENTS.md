# Requirements: AWS build flow with CloudWatch + self-termination + TTL cleanup

## Goals
- Run long builds on ephemeral EC2 instances started by GitHub Actions, without requiring GitHub Actions to stay alive for the full build.
- Stream build stdout/stderr to CloudWatch Logs so logs can be reviewed on demand.
- Notify by email when a build starts and when it finishes (success/failure/timeout).
- Ensure EC2 instances terminate themselves after the build finishes.
- Ensure any expired EC2 instances are terminated by a scheduled Lambda (TTL cleanup), even if the build or self-termination fails.

## Non-Goals
- No change to build logic itself (the build steps remain the same).
- No need to embed full build logs into email notifications.
- No multi-tenant or multi-project orchestration beyond the existing qrpc build flow.

## Assumptions
- Terraform is the source of truth for AWS resources.
- CloudWatch Logs retention should be 28 days.
- A single email address receives notifications.
- Instances are tagged with Project=qrpc-build-server (or a configurable project tag).
- Instances are launched from the existing launch template.

## High-Level Design
1) GitHub Actions launches an EC2 instance and sends an SSM command that starts the build in the background.
2) Build output is sent to CloudWatch Logs (log group: /qrpc/build).
3) Build script performs a self-termination on completion (success or failure).
4) A scheduled Lambda (EventBridge rule every 1 hour) terminates any EC2 instances whose ExpiresAt tag is in the past.
5) EventBridge listens to SSM Command status change events and sends email notifications via SNS on start and finish.

## Detailed Requirements

### A) CloudWatch Logs
- Create a CloudWatch Log Group named `/qrpc/build`.
- Retention: 28 days.
- SSM `send-command` must include CloudWatch output config:
  - CloudWatchOutputEnabled: true
  - CloudWatchLogGroupName: /qrpc/build

### B) Email Notifications
- Use SNS for email notifications.
- Create SNS Topic (e.g., `qrpc-build-notify`).
- Add a single email subscription (address configurable via Terraform variable).
- EventBridge rule(s) should send notifications on:
  - Command start (InProgress)
  - Command completion (Success, Failed, TimedOut, Cancelled)
- Notification payload should include at least:
  - Command ID
  - Instance ID
  - Status
  - Timestamp

### C) Background Build Execution
- SSM should start the build in the background (e.g., `nohup ... &`) to allow GitHub Actions to exit immediately.
- The background build must write its logs to stdout/stderr so CloudWatch Logs receives them.
- No requirement to stream logs back to GitHub Actions.

### D) Self-Termination
- After the build finishes (success or failure), the instance should terminate itself.
- Self-termination must be best-effort and not block final cleanup.
- Instance role must have permission to call `ec2:TerminateInstances` on itself.
- The instance should discover its own instance-id and region via IMDSv2.

### E) TTL / Expiration Cleanup
- Instances must be tagged with `ExpiresAt` (UTC ISO-8601), set at launch time to now + 24 hours.
- EventBridge rule runs every 1 hour.
- Lambda scans for instances with `ExpiresAt` in the past and terminates them.
- Lambda must filter to the expected project tag to avoid terminating unrelated instances.
- Lambda permissions:
  - `ec2:DescribeInstances`
  - `ec2:TerminateInstances`

### F) Terraform Implementation Notes
- All AWS resources added via Terraform:
  - CloudWatch Log Group
  - SNS Topic + Subscription
  - EventBridge rule(s) + permissions to SNS target
  - Lambda function + IAM role/policies
  - Any additional IAM permissions for EC2 instance role
- Terraform variables should include:
  - Notification email
  - Project tag (if not already present)
  - Log group name (optional, default `/qrpc/build`)

## Operational Flow (Proposed)
1) GitHub Actions:
   - Launch EC2 with tags including `ExpiresAt`.
   - Send SSM command to start build in background and write logs to stdout/stderr.
   - Exit immediately after command is accepted.
2) EventBridge -> SNS:
   - Sends email when SSM command transitions to InProgress and when it finishes.
3) CloudWatch Logs:
   - Logs available in `/qrpc/build` for on-demand viewing.
4) Self-termination:
   - Build script terminates its own instance at end.
5) TTL cleanup:
   - Hourly Lambda terminates any expired instances.

## Open Questions
- None.
