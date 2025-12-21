provider "aws" {
  region = var.aws_region
}

locals {
  bucket_name = var.bucket_name != "" ? var.bucket_name : null
}

data "aws_region" "current" {}

data "aws_caller_identity" "current" {}

data "aws_ami" "ubuntu_jammy_exact" {
  count       = var.ami_name != "" ? 1 : 0
  owners      = [var.ami_owner]
  most_recent = false

  filter {
    name   = "name"
    values = [var.ami_name]
  }

  filter {
    name   = "architecture"
    values = ["x86_64"]
  }
}

data "aws_ami_ids" "ubuntu_jammy_auto" {
  owners         = [var.ami_owner]
  sort_ascending = true
  name_regex     = var.ami_name_regex

  filter {
    name   = "architecture"
    values = ["x86_64"]
  }
}

data "aws_vpc" "default" {
  default = true
}

data "aws_subnets" "default" {
  filter {
    name   = "vpc-id"
    values = [local.vpc_id]
  }
}

locals {
  vpc_id    = var.vpc_id != "" ? var.vpc_id : data.aws_vpc.default.id
  subnet_id = var.subnet_id != "" ? var.subnet_id : data.aws_subnets.default.ids[0]
}

locals {
  ubuntu_base_ami_id = var.ami_name != "" ? data.aws_ami.ubuntu_jammy_exact[0].id : data.aws_ami_ids.ubuntu_jammy_auto.ids[0]
}

resource "aws_cloudwatch_log_group" "build" {
  name              = var.log_group_name
  retention_in_days = 30
  tags              = var.tags
}

resource "aws_sns_topic" "build_notify" {
  name = "${var.project_name}-build-notify"
  tags = var.tags
}

resource "aws_sns_topic_subscription" "build_notify_email" {
  topic_arn = aws_sns_topic.build_notify.arn
  protocol  = "email"
  endpoint  = var.notification_email
}

resource "aws_s3_bucket" "releases" {
  bucket        = local.bucket_name
  bucket_prefix = local.bucket_name == null ? "${var.project_name}-releases-" : null
  tags          = var.tags
}

resource "aws_s3_bucket_versioning" "releases" {
  bucket = aws_s3_bucket.releases.id

  versioning_configuration {
    status = "Suspended"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "releases" {
  bucket = aws_s3_bucket.releases.id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm = "AES256"
    }
  }
}

resource "aws_s3_bucket_public_access_block" "releases" {
  bucket                  = aws_s3_bucket.releases.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_cloudfront_origin_access_control" "releases" {
  name                              = "${var.project_name}-releases-oac"
  description                       = "OAC for ${var.project_name} release bucket"
  origin_access_control_origin_type = "s3"
  signing_behavior                  = "always"
  signing_protocol                  = "sigv4"
}

resource "aws_cloudfront_distribution" "releases" {
  enabled             = true
  comment             = "${var.project_name} release distribution"
  default_root_object = ""
  price_class         = "PriceClass_100"

  origin {
    domain_name              = aws_s3_bucket.releases.bucket_regional_domain_name
    origin_id                = "${var.project_name}-releases"
    origin_access_control_id = aws_cloudfront_origin_access_control.releases.id

    s3_origin_config {
      origin_access_identity = ""
    }
  }

  default_cache_behavior {
    target_origin_id       = "${var.project_name}-releases"
    viewer_protocol_policy = "redirect-to-https"
    allowed_methods        = ["GET", "HEAD"]
    cached_methods         = ["GET", "HEAD"]
    compress               = true

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    cloudfront_default_certificate = true
  }

  tags = var.tags
}

resource "aws_s3_bucket_policy" "releases" {
  bucket = aws_s3_bucket.releases.id
  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Sid       = "AllowCloudFrontRead",
        Effect    = "Allow",
        Principal = { Service = "cloudfront.amazonaws.com" },
        Action    = ["s3:GetObject"],
        Resource  = "${aws_s3_bucket.releases.arn}/*",
        Condition = {
          StringEquals = {
            "AWS:SourceArn" = aws_cloudfront_distribution.releases.arn
          }
        }
      }
    ]
  })
}

resource "aws_security_group" "build" {
  name        = "${var.project_name}-build-sg"
  description = "Allow egress-only for ${var.project_name} build server"
  vpc_id      = local.vpc_id

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = var.tags
}

resource "aws_iam_role" "build_instance" {
  name = "${var.project_name}-build-instance"

  assume_role_policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Principal = {
          Service = "ec2.amazonaws.com"
        },
        Action = "sts:AssumeRole"
      }
    ]
  })

  tags = var.tags
}

resource "aws_iam_role_policy" "build_s3" {
  name = "${var.project_name}-build-s3"
  role = aws_iam_role.build_instance.id

  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = [
          "s3:AbortMultipartUpload",
          "s3:GetBucketLocation",
          "s3:ListBucket"
        ],
        Resource = aws_s3_bucket.releases.arn
      },
      {
        Effect = "Allow",
        Action = [
          "s3:PutObject"
        ],
        Resource = "${aws_s3_bucket.releases.arn}/*"
      },
      {
        Effect = "Allow",
        Action = [
          "s3:GetObject"
        ],
        Resource = "${aws_s3_bucket.releases.arn}/*"
      }
    ]
  })
}

resource "aws_iam_role_policy" "build_logs" {
  name = "${var.project_name}-build-logs"
  role = aws_iam_role.build_instance.id

  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = [
          "logs:DescribeLogStreams",
          "logs:CreateLogStream",
          "logs:PutLogEvents"
        ],
        Resource = "${aws_cloudwatch_log_group.build.arn}:*"
      }
    ]
  })
}

resource "aws_iam_role_policy" "build_notify_sns" {
  name = "${var.project_name}-build-notify-sns"
  role = aws_iam_role.build_instance.id

  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = [
          "sns:Publish"
        ],
        Resource = aws_sns_topic.build_notify.arn
      }
    ]
  })
}

resource "aws_iam_role_policy" "build_self_terminate" {
  name = "${var.project_name}-build-self-terminate"
  role = aws_iam_role.build_instance.id

  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = [
          "ec2:TerminateInstances"
        ],
        Resource = "arn:aws:ec2:${var.aws_region}:${data.aws_caller_identity.current.account_id}:instance/*",
        Condition = {
          StringEquals = {
            "ec2:ResourceTag/Project" = var.project_name
          }
        }
      }
    ]
  })
}

resource "aws_iam_instance_profile" "build" {
  name = "${var.project_name}-build-profile"
  role = aws_iam_role.build_instance.name
}

resource "aws_iam_service_linked_role" "imagebuilder" {
  aws_service_name = "imagebuilder.amazonaws.com"
}

resource "aws_iam_role" "imagebuilder_instance" {
  name = "${var.project_name}-imagebuilder-instance"

  assume_role_policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Principal = {
          Service = "ec2.amazonaws.com"
        },
        Action = "sts:AssumeRole"
      }
    ]
  })

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "imagebuilder_instance_core" {
  role       = aws_iam_role.imagebuilder_instance.name
  policy_arn = "arn:aws:iam::aws:policy/EC2InstanceProfileForImageBuilder"
}

resource "aws_iam_role_policy_attachment" "imagebuilder_instance_ssm" {
  role       = aws_iam_role.imagebuilder_instance.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

resource "aws_iam_instance_profile" "imagebuilder" {
  name = "${var.project_name}-imagebuilder-profile"
  role = aws_iam_role.imagebuilder_instance.name
}

resource "aws_imagebuilder_component" "base_noop" {
  name     = "${var.project_name}-base"
  platform = "Linux"
  version  = "1.0.0"

  data = <<-EOF
    name: qrpc-base
    description: No-op component for golden AMI.
    schemaVersion: 1.0
    phases:
      - name: build
        steps:
          - name: noop
            action: ExecuteBash
            inputs:
              commands:
                - echo "qrpc base image"
  EOF

  tags = var.tags
}

resource "aws_imagebuilder_image_recipe" "build" {
  name         = "${var.project_name}-image-recipe"
  version      = "1.0.0"
  parent_image = local.ubuntu_base_ami_id

  component {
    component_arn = aws_imagebuilder_component.base_noop.arn
  }

  tags = var.tags
}

resource "aws_imagebuilder_infrastructure_configuration" "build" {
  name                          = "${var.project_name}-imagebuilder"
  instance_profile_name         = aws_iam_instance_profile.imagebuilder.name
  subnet_id                     = local.subnet_id
  security_group_ids            = [aws_security_group.build.id]
  terminate_instance_on_failure = true
  description                   = "Golden AMI build for ${var.project_name}"

  tags = var.tags
}

resource "aws_imagebuilder_distribution_configuration" "build" {
  name = "${var.project_name}-distribution"

  distribution {
    region = var.aws_region

    ami_distribution_configuration {
      name = "${var.project_name}-build-{{imagebuilder:buildDate}}"
      ami_tags = merge(var.tags, {
        Project     = var.project_name
        BaseAmiId   = local.ubuntu_base_ami_id
        ImageSource = "imagebuilder"
      })
    }
  }

  tags = var.tags
}

resource "aws_imagebuilder_image" "build" {
  image_recipe_arn                 = aws_imagebuilder_image_recipe.build.arn
  infrastructure_configuration_arn = aws_imagebuilder_infrastructure_configuration.build.arn
  distribution_configuration_arn   = aws_imagebuilder_distribution_configuration.build.arn

  tags = var.tags

  depends_on = [aws_iam_service_linked_role.imagebuilder]
}

locals {
  build_ami_id = one(tolist(one(aws_imagebuilder_image.build.output_resources).amis)).image
}

resource "aws_launch_template" "build" {
  name_prefix   = "${var.project_name}-build-"
  image_id      = local.build_ami_id
  instance_type = var.instance_type
  key_name      = var.key_name != "" ? var.key_name : null
  update_default_version = true

  iam_instance_profile {
    name = aws_iam_instance_profile.build.name
  }

  network_interfaces {
    subnet_id                   = local.subnet_id
    security_groups             = [aws_security_group.build.id]
    associate_public_ip_address = true
  }

  block_device_mappings {
    device_name = "/dev/sda1"

    ebs {
      delete_on_termination = true
      volume_size           = 64
    }
  }

  metadata_options {
    instance_metadata_tags = "enabled"
  }

  user_data = base64encode(<<-USER_DATA
    #!/usr/bin/env bash
    set -euo pipefail

    log_file="/var/log/qrpc-build-bootstrap.log"
    exec > >(tee -a "$log_file") 2>&1

    expected_kernel="6.8.0"
    expected_arch="x86_64"
    expected_version="22.04"

    actual_kernel="$(uname -r | cut -d- -f1)"
    actual_arch="$(uname -m)"
    actual_version=""

    if [ -r /etc/os-release ]; then
      . /etc/os-release
      actual_version="$${VERSION_ID:-}"
    fi

    if [ "$actual_kernel" != "$expected_kernel" ] || [ "$actual_arch" != "$expected_arch" ] || [ "$actual_version" != "$expected_version" ]; then
      echo "Build host requirements not met. kernel=$actual_kernel arch=$actual_arch version=$actual_version"
      shutdown -h now || true
      exit 1
    fi

    apt-get update -y
    apt-get install -y \
      awscli \
      bindfs \
      build-essential \
      curl \
      debootstrap \
      git \
      patch \
      rsync \
      wget

    cw_agent_deb="amazon-cloudwatch-agent.deb"
    curl -fSL "https://amazoncloudwatch-agent.s3.amazonaws.com/ubuntu/amd64/latest/$cw_agent_deb" -o "/tmp/$cw_agent_deb"
    dpkg -i "/tmp/$cw_agent_deb"

    mkdir -p /opt/qrpc-build

    cat > /opt/qrpc-build/build_package.sh <<'SCRIPT'
    #!/usr/bin/env bash
    set -euo pipefail

    self_terminate() {
      if ! command -v aws >/dev/null 2>&1; then
        return
      fi

      token="$(curl -sS -m 3 -X PUT "http://169.254.169.254/latest/api/token" \
        -H "X-aws-ec2-metadata-token-ttl-seconds: 300" || true)"
      if [ -z "$${token:-}" ]; then
        return
      fi

      instance_id="$(curl -sS -m 3 -H "X-aws-ec2-metadata-token: $token" \
        "http://169.254.169.254/latest/meta-data/instance-id" || true)"
      region="$(curl -sS -m 3 -H "X-aws-ec2-metadata-token: $token" \
        "http://169.254.169.254/latest/dynamic/instance-identity/document" \
        | sed -n 's/.*"region"[[:space:]]*:[[:space:]]*"\\([^"]*\\)".*/\\1/p')"

      if [ -n "$instance_id" ] && [ -n "$region" ]; then
        aws ec2 terminate-instances --region "$region" --instance-ids "$instance_id" >/dev/null 2>&1 || true
      fi
    }

    if [ "$${QRPC_SELF_TERMINATE:-1}" = "1" ]; then
      trap self_terminate EXIT
    fi

    usage() {
      echo "Usage: $0 <repo_url> <tag> <s3_bucket> <s3_prefix_unused>" >&2
    }

    if [ "$#" -lt 4 ]; then
      usage
      exit 1
    fi

    REPO_URL="$1"
    TAG="$2"
    S3_BUCKET="$3"
    S3_PREFIX_UNUSED="$4"

    notify_failure() {
      if [ -z "$${QRPC_SNS_TOPIC_ARN:-}" ]; then
        return
      fi
      local step="$1"
      local subject="qrpc build failed: $step"
      local message="Tag $TAG failed at step '$step' on $${QRPC_INSTANCE_ID:-unknown}."
      aws sns publish --topic-arn "$QRPC_SNS_TOPIC_ARN" --subject "$subject" --message "$message" >/dev/null 2>&1 || true
    }

    run_step() {
      local step="$1"
      shift
      if ! "$@"; then
        notify_failure "$step"
        exit 1
      fi
    }

    expected_kernel="6.8.0"
    expected_arch="x86_64"
    expected_version="22.04"

    actual_kernel="$(uname -r | cut -d- -f1)"
    actual_arch="$(uname -m)"
    actual_version=""

    if [ -r /etc/os-release ]; then
      . /etc/os-release
      actual_version="$${VERSION_ID:-}"
    fi

    if [ "$actual_kernel" != "$expected_kernel" ] || [ "$actual_arch" != "$expected_arch" ] || [ "$actual_version" != "$expected_version" ]; then
      echo "Build host requirements not met. kernel=$actual_kernel arch=$actual_arch version=$actual_version" >&2
      exit 1
    fi

    if ! command -v aws >/dev/null 2>&1; then
      echo "aws CLI missing on build host" >&2
      exit 1
    fi

    workdir="/src"
    repo_dir="$workdir/qrpc"

    mkdir -p "$workdir"
    rm -rf "$repo_dir"

    run_step "git clone" git clone --depth 1 --branch "$TAG" "$REPO_URL" "$repo_dir"

    cd "$repo_dir"

    export QRPC_BUILD_SERVER=1
    run_step "bootstrap" ./Build bootstrap
    previous_devel_key="$(aws s3api list-objects-v2 \
      --bucket "$S3_BUCKET" \
      --query "Contents[?contains(Key, '$${expected_arch}') && contains(Key, 'ubuntu$${expected_version}') && ends_with(Key, '-devel.tar.gz')]|sort_by(@,&LastModified)[-1].Key" \
      --output text 2>/dev/null || true)"

    if [ -n "$previous_devel_key" ] && [ "$previous_devel_key" != "None" ]; then
      tmp_dir="$(mktemp -d)"
      tmp_tar="$tmp_dir/qrpc-devel.tar.gz"
      run_step "download previous devel" aws s3 cp "s3://$S3_BUCKET/$previous_devel_key" "$tmp_tar"
      tar -zxf "$tmp_tar" -C "$tmp_dir"
      install_script="$(find "$tmp_dir" -maxdepth 2 -name install.sh -print -quit)"
      if [ -n "$install_script" ]; then
        chmod +x "$install_script"
        run_step "install previous devel" "$install_script"
      else
        echo "No install.sh found in previous devel package $previous_devel_key" >&2
        notify_failure "install previous devel"
        exit 1
      fi
      rm -rf "$tmp_dir"
    else
      echo "No previous devel package found; continuing without cache."
    fi

    run_step "devel" ./Build devel

    run_step "package" ./Build package
    run_step "package-devel" ./Build package-devel

    shopt -s nullglob
    packages=( _release_packages/*.tar.gz )
    if [ "$${#packages[@]}" -eq 0 ]; then
      echo "No packages found in _release_packages" >&2
      notify_failure "upload packages"
      exit 1
    fi

    for package_path in "$${packages[@]}"; do
      package_key="$(basename "$package_path")"
      run_step "upload $package_key" aws s3 cp "$package_path" "s3://$S3_BUCKET/$package_key"
    done

    printf "Uploaded %s package(s) to s3://%s\n" "$${#packages[@]}" "$S3_BUCKET"
    SCRIPT

    chmod +x /opt/qrpc-build/build_package.sh

    cat > /opt/qrpc-build/cloudwatch-agent.json <<'SCRIPT'
    {
      "logs": {
        "logs_collected": {
          "files": {
            "collect_list": [
              {
                "file_path": "/var/log/qrpc-build.log",
                "log_group_name": "${var.log_group_name}",
                "log_stream_name": "{instance_id}/qrpc-build"
              }
            ]
          }
        }
      }
    }
    SCRIPT

    cat > /opt/qrpc-build/start_build.sh <<'SCRIPT'
    #!/usr/bin/env bash
    set -euo pipefail

    log_file="/var/log/qrpc-build.log"
    skip_terminate_self=0

    token=""
    for _ in $(seq 1 5); do
      token="$(curl -sS -m 3 -X PUT "http://169.254.169.254/latest/api/token" \
        -H "X-aws-ec2-metadata-token-ttl-seconds: 300" || true)"
      if [ -n "$${token:-}" ]; then
        break
      fi
      sleep 1
    done
    if [ -z "$${token:-}" ]; then
      echo "Failed to acquire IMDSv2 token." >&2
      shutdown -h now || true
      exit 1
    fi

    instance_id="$(curl -sS -m 3 -H "X-aws-ec2-metadata-token: $token" \
      "http://169.254.169.254/latest/meta-data/instance-id" || true)"
    region="${var.aws_region}"
    if [ -n "$${region:-}" ]; then
      export AWS_REGION="$region"
      export AWS_DEFAULT_REGION="$region"
    fi

    terminate_self() {
      if [ "$skip_terminate_self" = "1" ]; then
        return
      fi
      if ! command -v aws >/dev/null 2>&1; then
        shutdown -h now || true
        return
      fi

      if [ -z "$${instance_id:-}" ]; then
        instance_id="$(curl -sS -m 3 -H "X-aws-ec2-metadata-token: $token" \
          "http://169.254.169.254/latest/meta-data/instance-id" || true)"
      fi
      if [ -z "$${region:-}" ]; then
        region="$(curl -sS -m 3 -H "X-aws-ec2-metadata-token: $token" \
          "http://169.254.169.254/latest/dynamic/instance-identity/document" \
          | sed -n 's/.*"region"[[:space:]]*:[[:space:]]*"\\([^"]*\\)".*/\\1/p')"
      fi
      if [ -n "$${instance_id:-}" ] && [ -n "$${region:-}" ]; then
        aws ec2 terminate-instances --region "$region" --instance-ids "$instance_id" >/dev/null 2>&1 || true
        return
      fi

      shutdown -h now || true
    }

    trap terminate_self EXIT

    topic_arn="${aws_sns_topic.build_notify.arn}"

    publish_status() {
      if ! command -v aws >/dev/null 2>&1; then
        return
      fi
      if [ -z "$${topic_arn:-}" ]; then
        return
      fi
      local subject="qrpc build $1"
      local message="$2"
      aws sns publish --topic-arn "$topic_arn" --subject "$subject" --message "$message" >/dev/null 2>&1 || true
    }

    tag=""
    for _ in $(seq 1 30); do
      tag="$(curl -sS -m 3 -H "X-aws-ec2-metadata-token: $token" \
        "http://169.254.169.254/latest/meta-data/tags/instance/Tag" || true)"
      if [ -n "$${tag:-}" ]; then
        break
      fi
      sleep 2
    done
    if [ -z "$${tag:-}" ]; then
      echo "Missing instance Tag value; refusing to run build." >&2
      terminate_self
      exit 1
    fi

    repo_url="https://github.com/${var.github_repository}.git"
    s3_bucket="${aws_s3_bucket.releases.bucket}"

    export QRPC_SNS_TOPIC_ARN="$topic_arn"
    export QRPC_INSTANCE_ID="$instance_id"

    {
      echo "==== start_build.sh (embedded) ===="
      cat /opt/qrpc-build/start_build.sh
      echo "==== build_package.sh (embedded) ===="
      cat /opt/qrpc-build/build_package.sh
    } >> "$log_file"

    set +e
    /opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl \
      -a fetch-config \
      -m ec2 \
      -c file:/opt/qrpc-build/cloudwatch-agent.json \
      -s
    cw_status="$?"
    set -e
    # If log shipping is broken, shut down without attempting API termination to avoid
    # losing the local logs needed for debugging.
    if [ "$cw_status" -ne 0 ]; then
      skip_terminate_self=1
      echo "CloudWatch Agent failed to start; shutting down for manual log recovery." >> "$log_file"
      shutdown -h now || true
      exit 1
    fi

    publish_status "started" "Tag $tag started on $${instance_id:-unknown}."

    set +e
    QRPC_SELF_TERMINATE=0 /opt/qrpc-build/build_package.sh "$repo_url" "$tag" "$s3_bucket" "unused" \
      > "$log_file" 2>&1
    status="$?"
    set -e

    if [ "$status" -eq 0 ]; then
      publish_status "succeeded" "Tag $tag finished on $${instance_id:-unknown}."
    fi

    terminate_self
    exit "$status"
    SCRIPT

    chmod +x /opt/qrpc-build/start_build.sh
    /opt/qrpc-build/start_build.sh
  USER_DATA
  )

  tags = var.tags
}

data "archive_file" "ttl_cleanup" {
  type        = "zip"
  source_file = "${path.module}/lambda/ttl_cleanup.py"
  output_path = "${path.module}/lambda/ttl_cleanup.zip"
}

resource "aws_iam_role" "ttl_cleanup_lambda" {
  name = "${var.project_name}-ttl-cleanup"

  assume_role_policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Principal = {
          Service = "lambda.amazonaws.com"
        },
        Action = "sts:AssumeRole"
      }
    ]
  })

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "ttl_cleanup_lambda_logs" {
  role       = aws_iam_role.ttl_cleanup_lambda.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}

resource "aws_iam_role_policy" "ttl_cleanup_lambda_ec2" {
  name = "${var.project_name}-ttl-cleanup-ec2"
  role = aws_iam_role.ttl_cleanup_lambda.id

  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = [
          "ec2:DescribeInstances"
        ],
        Resource = "*"
      },
      {
        Effect = "Allow",
        Action = [
          "ec2:TerminateInstances"
        ],
        Resource = "arn:aws:ec2:${var.aws_region}:${data.aws_caller_identity.current.account_id}:instance/*",
        Condition = {
          StringEquals = {
            "ec2:ResourceTag/Project" = var.project_name
          }
        }
      }
    ]
  })
}

resource "aws_lambda_function" "ttl_cleanup" {
  function_name    = "${var.project_name}-ttl-cleanup"
  role             = aws_iam_role.ttl_cleanup_lambda.arn
  handler          = "ttl_cleanup.handler"
  runtime          = "python3.11"
  filename         = data.archive_file.ttl_cleanup.output_path
  source_code_hash = data.archive_file.ttl_cleanup.output_base64sha256
  timeout          = 60

  environment {
    variables = {
      PROJECT_TAG     = var.project_name
      PROJECT_TAG_KEY = "Project"
      EXPIRES_AT_TAG  = "ExpiresAt"
    }
  }

  tags = var.tags
}

resource "aws_cloudwatch_event_rule" "ttl_cleanup_schedule" {
  name                = "${var.project_name}-ttl-cleanup"
  schedule_expression = "rate(1 hour)"
}

resource "aws_cloudwatch_event_target" "ttl_cleanup_lambda" {
  rule = aws_cloudwatch_event_rule.ttl_cleanup_schedule.name
  arn  = aws_lambda_function.ttl_cleanup.arn
}

resource "aws_lambda_permission" "ttl_cleanup" {
  statement_id  = "AllowEventBridgeInvoke"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.ttl_cleanup.function_name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.ttl_cleanup_schedule.arn
}

resource "aws_iam_openid_connect_provider" "github" {
  url             = "https://token.actions.githubusercontent.com"
  client_id_list  = ["sts.amazonaws.com"]
  thumbprint_list = [var.github_oidc_thumbprint]
}

resource "aws_iam_role" "github_actions" {
  name = "${var.project_name}-github-actions"

  assume_role_policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Principal = {
          Federated = aws_iam_openid_connect_provider.github.arn
        },
        Action = "sts:AssumeRoleWithWebIdentity",
        Condition = {
          StringEquals = {
            "token.actions.githubusercontent.com:aud" = "sts.amazonaws.com"
          },
          StringLike = {
            "token.actions.githubusercontent.com:sub" = "repo:${var.github_repository}:ref:refs/tags/*"
          }
        }
      }
    ]
  })

  tags = var.tags
}

resource "aws_iam_role_policy" "github_actions" {
  name = "${var.project_name}-github-actions-policy"
  role = aws_iam_role.github_actions.id

  policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
      {
        Effect = "Allow",
        Action = [
          "ec2:DescribeInstances"
        ],
        Resource = "*",
        Condition = {
          StringEquals = {
            "aws:RequestedRegion" = var.aws_region
          }
        }
      },
      {
        Effect = "Allow",
        Action = [
          "ec2:RunInstances"
        ],
        Resource = "*",
        Condition = {
          StringEquals = {
            "ec2:LaunchTemplate" = aws_launch_template.build.arn,
            "aws:RequestedRegion" = var.aws_region
          },
          "ForAllValues:StringEquals" = {
            "aws:TagKeys" = [
              "Project",
              "Name",
              "GitHubRun",
              "Tag",
              "ExpiresAt"
            ]
          }
        }
      },
      {
        Effect = "Allow",
        Action = "ec2:CreateTags",
        Resource = "arn:aws:ec2:${var.aws_region}:${data.aws_caller_identity.current.account_id}:instance/*",
        Condition = {
          StringEquals = {
            "ec2:CreateAction" = "RunInstances",
            "aws:RequestTag/Project" = var.project_name,
            "aws:RequestedRegion" = var.aws_region
          },
          "ForAllValues:StringEquals" = {
            "aws:TagKeys" = [
              "Project",
              "Name",
              "GitHubRun",
              "Tag",
              "ExpiresAt"
            ]
          }
        }
      },
      {
        Effect = "Allow",
        Action = "iam:PassRole",
        Resource = aws_iam_role.build_instance.arn,
        Condition = {
          StringEquals = {
            "iam:PassedToService" = "ec2.amazonaws.com"
          }
        }
      }
    ]
  })
}
