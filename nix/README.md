# NIX

My NixOS configuration for various devices.

### structure

* A **machine** has one or more role.
* A **role** is a collection of **packages** and **services**.

### secrets.nix

Secrets are stored in `secrets.nix` in the following form:

```
{
  users = [
    {
      name = "someone";
      isNormalUser = true;
      description = "Some One";
      hashedPassword = "secret password for Some One";
      ...
    }
    ...
  ];
}
```

### configuration.nix

```
{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./machines/some-machine.nix
    ];
}
```
