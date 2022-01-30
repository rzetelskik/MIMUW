obj-m += atdd.o
atdd-objs += atomic_disc_driver.o atdd_tcp_client.o parse_args.o tcp.o

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules
clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean
