let

  region = "us-east-1";
  ami = "ami-5e347649";
  accessKeyId = "dev";
  instanceType = "m1.small";

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.ami = ami;
      # deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.regions = region;
      deployment.ec2.instanceType = instanceType;
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    };

in

{ proxy = ec2;
  backend1 = ec2;
  backend2 = ec2;

  # provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };

}
