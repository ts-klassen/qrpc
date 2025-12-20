# AWS build + distribution

This directory provisions the EC2 build server, S3 bucket, and CloudFront distribution needed to build and distribute release artifacts.

## What it does
- EC2 build server pinned to Ubuntu 22.04 (Jammy) with kernel 5.15.0 on x86_64.
- CloudWatch Logs group for build output and SNS topic for build start/finish notifications.
- Scheduled TTL cleanup via Lambda for expired build instances.
- S3 bucket to store release artifacts.
- CloudFront distribution to serve artifacts over HTTPS.
- Image Builder creates a golden AMI from the oldest matching Ubuntu 22.04 base.
- IAM roles for the EC2 instance and GitHub Actions OIDC.

## Terraform apply
From `aws/terraform`:

```bash
terraform init
terraform apply \
  -var="github_repository=YOUR_ORG/YOUR_REPO" \
  -var="aws_region=us-east-1" \
  -var="notification_email=you@example.com"
```

Optional variables you may want to set:
- `bucket_name` to force a fixed S3 bucket name.
- `subnet_id` / `vpc_id` to place the build server in a specific network.
- `key_name` for SSH access.
- `ami_name` to pin a specific Ubuntu 22.04 AMI name.
- `ami_name_regex` to control which AMIs are considered for auto-selection.
- `log_group_name` to override the CloudWatch Logs group (default `/qrpc/build`).

## GitHub Actions secrets
Add these secrets to the repository:
- `AWS_GITHUB_ROLE_ARN`: output `github_actions_role_arn` from Terraform.
- `AWS_REGION`: the same region used by Terraform.
- `AWS_BUILD_LAUNCH_TEMPLATE_ID`: output `build_launch_template_id` from Terraform.

## Release flow
1. Push a tag that points to a commit on `main`.
2. The workflow launches an EC2 instance from the launch template.
3. The instance reads its `Tag` value from IMDS and runs the build script.
4. The instance is terminated after the build finishes (or fails), deleting its root volume.
5. The built tarball is uploaded to `s3://<bucket>/` (bucket root).
6. The devel tarball is uploaded to `s3://<bucket>/` (bucket root).
7. CloudFront serves the bucket via HTTPS at the output `cloudfront_domain_name`.

## Notes
- The EC2 instance bootstrap script validates the kernel (`5.15.0`), architecture (`x86_64`), and Ubuntu version (`22.04`). It shuts down if they do not match.
- The build script is installed on each EC2 instance at `/opt/qrpc-build/build_package.sh`, so GitHub Actions does not need S3 access.
- The bootstrap reads the `Tag` instance tag via IMDS and uses fixed repo + bucket values baked into the launch template.
- The workflow tags instances with `Project=qrpc-build-server`, `Tag`, and `ExpiresAt`; if you change `project_name` in Terraform, update the workflow tag to match.
- The SNS topic sends a subscription confirmation email on first apply; confirm it before notifications are delivered.
- Build start/finish notifications are published directly by the instance to the SNS topic.
- Step-level failures (bootstrap/devel/package/upload) also publish SNS notifications.
- The bootstrap configures CloudWatch Agent to ship `/var/log/qrpc-build.log` to the log group; you can watch with `aws logs tail`.
- Before running `./Build devel`, the build script pulls the most recent devel tarball from S3 and runs its `install.sh` to avoid rebuilding unchanged packages.
- Image Builder runs only when Terraform applies; the AMI is not refreshed automatically.
