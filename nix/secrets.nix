{
  users = [
    # {
    #   name = "root";
    #   extraGroups = [];
    #   isNormalUser = false;
    #   openssh.authorizedKeys.keys = [
    #     # add public SSH keys here
    #   ];
    # }
    {
      name = "placek";
      uid = 1000;
      isNormalUser = true;
      description = "Paweł Placzyński";
      # hashedPassword = "secret password for that user";
    }
    # and so on...
  ];
}
