# Manual build test from AWS CloudShell (us-east-1)

This runs the same flow as GitHub Actions: launch an ephemeral EC2 instance from the launch template, boot-time script starts the build, upload to S3, then terminate the instance.

## 1) Terraform apply and capture outputs
```bash
cd aws/terraform
terraform init
terraform apply -var="github_repository=YOUR_ORG/YOUR_REPO" -var="aws_region=us-east-1"
```

```bash
export AWS_REGION=us-east-1
export LAUNCH_TEMPLATE_ID="$(terraform output -raw build_launch_template_id)"
export BUILD_BUCKET="$(terraform output -raw release_bucket_name)"
export LOG_GROUP="$(terraform output -raw build_log_group_name)"
export PROJECT_TAG="qrpc-build-server"
export TAG="vX.Y.Z"
```

Note: Terraform now auto-selects the oldest matching Ubuntu 22.04 AMI and
builds a golden AMI with Image Builder. This can add several minutes to
`terraform apply`. Use `ami_name` to pin a specific image or `ami_name_regex`
to change the selection filter.

## 2) Launch an instance from the launch template
```bash
INSTANCE_ID=$(aws ec2 run-instances \
  --launch-template "LaunchTemplateId=$LAUNCH_TEMPLATE_ID" \
  --tag-specifications "ResourceType=instance,Tags=[{Key=Name,Value=qrpc-build-server},{Key=Project,Value=$PROJECT_TAG},{Key=ManualTest,Value=true},{Key=Tag,Value=$TAG},{Key=ExpiresAt,Value=$(date -u -d '+24 hours' '+%Y-%m-%dT%H:%M:%SZ')}]" \
  --query "Instances[0].InstanceId" \
  --output text)

echo "Instance: $INSTANCE_ID"
aws ec2 wait instance-running --instance-ids "$INSTANCE_ID"
```

## 3) Build runs automatically on boot
```bash
echo "Build starts automatically; logs are written to /var/log/qrpc-build.log on the instance."
echo "CloudWatch Logs also streams /var/log/qrpc-build.log (log group: $LOG_GROUP)."
```

## 4) Tail CloudWatch Logs (optional)
```bash
aws logs tail "$LOG_GROUP" --since 10m --follow
```

## 5) Verify S3 uploads
```bash
aws s3 ls "s3://$BUILD_BUCKET/"
```

## 6) Terminate instance (and delete its root volume)
The instance will self-terminate after the build. Use this only if you need to clean up manually.
```bash
aws ec2 terminate-instances --instance-ids "$INSTANCE_ID"
aws ec2 wait instance-terminated --instance-ids "$INSTANCE_ID"
```
