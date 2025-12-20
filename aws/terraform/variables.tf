variable "aws_region" {
  type        = string
  description = "AWS region to deploy into."
  default     = "us-east-1"
}

variable "project_name" {
  type        = string
  description = "Prefix for naming AWS resources."
  default     = "qrpc-build-server"
}

variable "bucket_name" {
  type        = string
  description = "Optional fixed S3 bucket name for release artifacts. Leave empty to auto-generate."
  default     = ""
}

variable "ami_name" {
  type        = string
  description = "Exact Ubuntu 22.04 AMI name (oldest Jammy release) to enforce kernel 5.15.0."
  default     = "ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-20220420"
}

variable "ami_owner" {
  type        = string
  description = "AMI owner for Canonical Ubuntu images."
  default     = "099720109477"
}

variable "instance_type" {
  type        = string
  description = "EC2 instance type for the build server."
  default     = "t3.large"
}

variable "subnet_id" {
  type        = string
  description = "Optional subnet ID for the build server. Leave empty to use the default VPC subnet."
  default     = ""
}

variable "vpc_id" {
  type        = string
  description = "Optional VPC ID for the build server. Leave empty to use the default VPC."
  default     = ""
}

variable "key_name" {
  type        = string
  description = "Optional EC2 key pair name for SSH access."
  default     = ""
}

variable "github_repository" {
  type        = string
  description = "GitHub repo in owner/name form for OIDC trust."
}

variable "github_oidc_thumbprint" {
  type        = string
  description = "Thumbprint for GitHub OIDC provider."
  default     = "6938fd4d98bab03faadb97b34396831e3780aea1"
}

variable "tags" {
  type        = map(string)
  description = "Common tags applied to resources."
  default     = {}
}
