{
  users = [
    {
      name = "placek";
      uid = 1000;
      isNormalUser = true;
      description = "Paweł Placzyński";
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCvEijquT2tI7ROEORi7k0Nah+73U5YItParUhwdvvDYDlP6y41pZgnc5YBZKFX22kWfJLTHHcemagoZ2j0I4UZi3bl7djkRhy8rPlDmmLy03zfnHD0OVWLagXpWNhKyhlF1uq08ksdtHkL/ZOwA2OrWOJZMFM07g6iqoR+nke/LzH43tkOdDFwh0H7GgfAF4R7yht5Lsla6su6v+O8lj1wLAgEeG1ApskpxjbJG8eZJeLRRdtsFeeQWYCf0IGkDUHAcffOH5odcIr8w2Xbn3ZBD9tBvmXVVMpwG3iDC2IPUl9C1D+nhUDRKpte772M3TWjygHpprQtKhL5w+KAFVm6O4ha1X2mka2jEsvDkYwIOsvdWdPWNzC0DQAqUsZ/iJpZxni3IVu35ekPAyN7Kl/Yt/PdCpFWq4SloVnLYlSNQWjtxOs5LnqwwjCAzfdwUZ0hvvYRzK9Egi/PnONWQ1a7c2ePMneoR0sdrzgCkYpvt5ec1kOssoHzVNnneAbfpFU= placek@placek-macbook-pro"
      ];
      # hashedPassword = "secret password for that user";
    }
  ];
}
