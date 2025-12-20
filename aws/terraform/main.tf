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

resource "aws_iam_role_policy_attachment" "build_ssm" {
  role       = aws_iam_role.build_instance.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
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
    }
  }

  user_data = base64encode(<<-USER_DATA
    #!/usr/bin/env bash
    set -euo pipefail

    log_file="/var/log/qrpc-build-bootstrap.log"
    exec > >(tee -a "$log_file") 2>&1

    expected_kernel="5.15.0"
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
    apt-get install -y \\
      awscli \\
      bindfs \\
      build-essential \\
      curl \\
      debootstrap \\
      git \\
      patch \\
      rsync \\
      wget

    systemctl enable --now snap.amazon-ssm-agent.amazon-ssm-agent || true
    systemctl enable --now amazon-ssm-agent || true

    mkdir -p /opt/qrpc-build

    cat > /opt/qrpc-build/build_package.sh <<'SCRIPT'
    #!/usr/bin/env bash
    set -euo pipefail

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

    expected_kernel="5.15.0"
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

    workdir="/opt/qrpc-build"
    repo_dir="$workdir/qrpc"

    mkdir -p "$workdir"
    rm -rf "$repo_dir"

    git clone --depth 1 --branch "$TAG" "$REPO_URL" "$repo_dir"

    cd "$repo_dir"

    export QRPC_BUILD_SERVER=1
    ./Build bootstrap
    previous_devel_key="$(aws s3api list-objects-v2 \
      --bucket "$S3_BUCKET" \
      --query "Contents[?ends_with(Key, '-devel.tar.gz')]|sort_by(@,&LastModified)[-1].Key" \
      --output text 2>/dev/null || true)"

    if [ -n "$previous_devel_key" ] && [ "$previous_devel_key" != "None" ]; then
      tmp_dir="$(mktemp -d)"
      tmp_tar="$tmp_dir/qrpc-devel.tar.gz"
      aws s3 cp "s3://$S3_BUCKET/$previous_devel_key" "$tmp_tar"
      tar -zxf "$tmp_tar" -C "$tmp_dir"
      install_script="$(find "$tmp_dir" -maxdepth 2 -name install.sh -print -quit)"
      if [ -n "$install_script" ]; then
        chmod +x "$install_script"
        "$install_script"
      else
        echo "No install.sh found in previous devel package $previous_devel_key" >&2
        exit 1
      fi
      rm -rf "$tmp_dir"
    else
      echo "No previous devel package found; continuing without cache."
    fi

    ./Build devel

    package_path="$(./Build package | tail -n 1)"

    if [ ! -f "$package_path" ]; then
      echo "Package not found at $package_path" >&2
      exit 1
    fi

    package_key="$(basename "$package_path")"
    aws s3 cp "$package_path" "s3://$S3_BUCKET/$package_key"

    devel_path="$(./Build package-devel | tail -n 1)"

    if [ ! -f "$devel_path" ]; then
      echo "Devel package not found at $devel_path" >&2
      exit 1
    fi

    devel_key="$(basename "$devel_path")"
    aws s3 cp "$devel_path" "s3://$S3_BUCKET/$devel_key"

    printf "Uploaded %s to s3://%s/%s\n" "$package_path" "$S3_BUCKET" "$package_key"
    SCRIPT

    chmod +x /opt/qrpc-build/build_package.sh
  USER_DATA
  )

  tags = var.tags
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
          "ssm:SendCommand",
          "ssm:GetCommandInvocation"
        ],
        Resource = "arn:aws:ssm:${var.aws_region}:${data.aws_caller_identity.current.account_id}:document/AWS-RunShellScript"
      },
      {
        Effect = "Allow",
        Action = [
          "ssm:SendCommand",
          "ssm:GetCommandInvocation"
        ],
        Resource = "arn:aws:ec2:${var.aws_region}:${data.aws_caller_identity.current.account_id}:instance/*",
        Condition = {
          StringEquals = {
            "ssm:resourceTag/Project" = var.project_name
          }
        }
      },
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
          "ec2:RunInstances"
        ],
        Resource = "*",
        Condition = {
          StringEquals = {
            "ec2:LaunchTemplate" = aws_launch_template.build.arn
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
            "aws:RequestTag/Project" = var.project_name
          },
          "ForAllValues:StringEquals" = {
            "aws:TagKeys" = [
              "Project",
              "Name",
              "GitHubRun",
              "Repo",
              "Tag"
            ]
          }
        }
      },
      {
        Effect = "Allow",
        Action = "ec2:TerminateInstances",
        Resource = "arn:aws:ec2:${var.aws_region}:${data.aws_caller_identity.current.account_id}:instance/*",
        Condition = {
          StringEquals = {
            "ec2:ResourceTag/Project" = var.project_name
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
