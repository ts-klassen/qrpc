# Manual build test from AWS CloudShell (us-east-1)

This runs the same flow as GitHub Actions: launch an ephemeral EC2 instance from the launch template, build via SSM, upload to S3, then terminate the instance.

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
export PROJECT_TAG="qrpc-build-server"
```

Note: Terraform now auto-selects the oldest matching Ubuntu 22.04 AMI and
builds a golden AMI with Image Builder. This can add several minutes to
`terraform apply`. Use `ami_name` to pin a specific image or `ami_name_regex`
to change the selection filter.

## 2) Launch an instance from the launch template
```bash
INSTANCE_ID=$(aws ec2 run-instances \
  --launch-template "LaunchTemplateId=$LAUNCH_TEMPLATE_ID" \
  --tag-specifications "ResourceType=instance,Tags=[{Key=Name,Value=qrpc-build-server},{Key=Project,Value=$PROJECT_TAG},{Key=ManualTest,Value=true}]" \
  --query "Instances[0].InstanceId" \
  --output text)

echo "Instance: $INSTANCE_ID"
aws ec2 wait instance-running --instance-ids "$INSTANCE_ID"
```

## 3) Run the build via SSM
```bash
REPO_URL="https://github.com/YOUR_ORG/YOUR_REPO.git"
TAG="vX.Y.Z"
COMMAND_ID=$(aws ssm send-command \
  --instance-ids "$INSTANCE_ID" \
  --document-name "AWS-RunShellScript" \
  --comment "Manual build test for $TAG" \
  --parameters commands="[/opt/qrpc-build/build_package.sh '$REPO_URL' '$TAG' '$BUILD_BUCKET' 'unused']" \
  --query "Command.CommandId" \
  --output text)

echo "SSM command: $COMMAND_ID"
aws ssm wait command-executed --command-id "$COMMAND_ID" --instance-id "$INSTANCE_ID"
```

## 4) Inspect command output
```bash
aws ssm get-command-invocation --command-id "$COMMAND_ID" --instance-id "$INSTANCE_ID"
```

## 5) Verify S3 uploads
```bash
aws s3 ls "s3://$BUILD_BUCKET/"
```

## 6) Terminate instance (and delete its root volume)
```bash
aws ec2 terminate-instances --instance-ids "$INSTANCE_ID"
aws ec2 wait instance-terminated --instance-ids "$INSTANCE_ID"
```
