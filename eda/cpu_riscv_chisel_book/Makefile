PROJECT_NAME := cpu_riscv_chisel_book

TARGET ?= tangnano9k_pmod
CHISEL_TEMPLATE_DIR ?= ../../external/riscv-chisel-book/chisel-template
ROOT_DIR ?= $(abspath ../..)
RTL_DIR ?= $(ROOT_DIR)/rtl
CHISEL_DESIGN_DIR ?= $(ROOT_DIR)/cpu_design/src/main/scala
CHISEL_DESIGN_SRCS := $(shell find $(CHISEL_DESIGN_DIR) -name "*.scala")

BOOTROM_HEX ?= src/sw/bootrom.hex
#BOOTROM_HEX ?= src/sw-rs/bootrom.hex

RISCV_ELABORATE := fpga.Elaborate_Minimal 
RISCV_ELABORATE_OUTPUT_DIR := src/$(TARGET)/chisel_output

RISCV_CORE_SRC := $(RISCV_ELABORATE_OUTPUT_DIR)/Top.sv
PROJECT_ADDITIONAL_ARGS := $(abspath $(RISCV_CORE_SRC))
PROJECT_ADDITIONAL_CLEAN := $(RISCV_CORE_SRC)

SRCS := $(wildcard src/$(TARGET)/*.cst) $(wildcard src/$(TARGET)/*.sdc) $(wildcard src/$(TARGET)/*.sv) $(RISCV_CORE_SRC) project.tcl $(BOOTROM_HEX)

include ../build_gowin.mk

$(RISCV_CORE_SRC): $(CHISEL_DESIGN_SRCS) $(BOOTROM_HEX)
	cd $(ROOT_DIR) && sbt "project cpu_design; runMain $(RISCV_ELABORATE) $(dir $(abspath $@)) 8192 $(abspath $(dir $(BOOTROM_HEX)))"

src/sw/bootrom.hex: src/sw/bootrom.c src/sw/link.ld src/sw/Makefile
	cd src/sw; make

src/sw-rs/bootrom.hex: $(wildcard src/sw-rs/src/*.rs) src/sw-rs/pac/build.rs src/sw-rs/pac/cpu_riscv_chisel_book.svd
	cd src/sw-rs; make