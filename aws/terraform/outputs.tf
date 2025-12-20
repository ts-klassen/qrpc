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

output "github_actions_role_arn" {
  value       = aws_iam_role.github_actions.arn
  description = "IAM role to assume from GitHub Actions via OIDC."
}
