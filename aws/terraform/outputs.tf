output "release_bucket_name" {
  value       = aws_s3_bucket.releases.bucket
  description = "S3 bucket storing release artifacts."
}

output "cloudfront_domain_name" {
  value       = aws_cloudfront_distribution.releases.domain_name
  description = "CloudFront domain for HTTPS downloads."
}

output "build_launch_template_id" {
  value       = aws_launch_template.build.id
  description = "Launch template ID for ephemeral build instances."
}

output "build_ami_id" {
  value       = one(tolist(one(aws_imagebuilder_image.build.output_resources).amis)).image
  description = "AMI ID created by Image Builder for build instances."
}

output "build_log_group_name" {
  value       = aws_cloudwatch_log_group.build.name
  description = "CloudWatch Log Group for build output."
}

output "build_notify_topic_arn" {
  value       = aws_sns_topic.build_notify.arn
  description = "SNS topic ARN for build start/finish notifications."
}

output "build_ssm_document_name" {
  value       = aws_ssm_document.build_run_shell.name
  description = "SSM document name for running build commands."
}

output "github_actions_role_arn" {
  value       = aws_iam_role.github_actions.arn
  description = "IAM role to assume from GitHub Actions via OIDC."
}
