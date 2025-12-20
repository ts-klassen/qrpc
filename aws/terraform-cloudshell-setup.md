# Terraform install and PATH persistence for AWS CloudShell

This guide installs Terraform in AWS CloudShell and makes the PATH change persist across sessions.

## 1) Confirm Terraform is missing (optional)
```bash
which terraform
```

## 2) Download and install Terraform
```bash
TF_VERSION=1.14.3
curl -L -o /tmp/terraform.zip \
  https://releases.hashicorp.com/terraform/${TF_VERSION}/terraform_${TF_VERSION}_linux_amd64.zip

mkdir -p ~/.local/bin
unzip -o /tmp/terraform.zip -d ~/.local/bin
```

## 3) Verify
```bash
terraform -version
which terraform
```
