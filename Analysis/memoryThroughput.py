import math
import argparse
from dataclasses import dataclass
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter


@dataclass
class Board:
    name: str
    pcie_throughput: float
    local_memory: float
    freq: float

    def __init__(self, name, pcie_throughput, local_memory, freq):
        self.name = name
        self.pcie_throughput = pcie_throughput * 8
        self.local_memory = local_memory * 8
        self.freq = freq


EUROPE_AREA_HA = 10_186_000 * 100
SWEDEN_AREA_HA = 447_425 * 100
GOTLAND_AREA_HA = 4_184 * 100
AVERAGE_ANIMAL_PER_CELL = 0.25

MIN_LANES = 100
MAX_LANES = 500

HALO_STEPS = np.asarray([0, 1, 4, 10, 20, 40, 60, 80, 100, 200])

COMET_065B = Board("Comet A65", 8e9, 16e9, 300e6)
DE10 = Board("DE10-Agilex", 64e9, 32e9, 700e6)
HBM2E_STARTER = Board("HBM2e Dev-kit", 128e9, 48e9, 850e6)
# One DDR4 socket is shared with HPS, so make space for 4Gb for that
MERCURY_A2700 = Board("Mercury A2700 Accelerator", 128e9, 32e9 * 4 - 4e9, 700e6)
AGILEX7_STARTER = Board("Agilex 7 Starter Kit", 16e9, 16e9, 700e6)


def memory_per_s(freq, lanes, animal_size, cell_size):
    # Assume the calculation of one animal tick correspond to the average nr of cell ticks
    # being done too. This only holds if cell updates are very cheap
    return lanes * freq * (animal_size + cell_size / AVERAGE_ANIMAL_PER_CELL)


def show_fpga_througput(lanes, animal_sizes):

    fig, ax = plt.subplots()
    for animal_size in animal_sizes:
        ax.plot(
            lanes,
            memory_per_s(COMET_065B.freq, lanes, animal_size, 32),
            label=f"size {animal_size}",
        )

    # / 2 due to off-board RAM hosting requires transfer back and forth to be usable
    ax.axhline(y=COMET_065B.pcie_throughput, label="PCIE Throughput Max")
    ax.yaxis.set_major_formatter(FuncFormatter(bits_to_gb))

    ax.set_title("Effective free memory on host")
    ax.set_xlim(MIN_LANES, MAX_LANES)
    ax.set_xlabel("Lanes")
    ax.set_ylabel("Generated Data (Gb)")
    ax.legend()

    plt.show()


def memory_for_halo(nr_animals, animal_size, cell_size, halo_steps, halo_step_size):
    nr_cells = AVERAGE_ANIMAL_PER_CELL / nr_animals

    # TODO: Adapt for the rhomb that the hexgrid is actually on instead of falsely
    # assuming a square
    square_side = int(math.ceil(math.sqrt(nr_cells)))

    return (
        square_side
        * halo_steps
        * halo_step_size
        * (cell_size + AVERAGE_ANIMAL_PER_CELL * animal_size)
    )


# TODO: Add better description of what this is meant to achieve
def effective_mem_available_on_host(board: Board, lanes, halo_steps):
    animal_size = 128
    cell_size = 32

    nr_animals = board.local_memory / animal_size
    # TODO: Display what size area this would mean and compare to some known region sizes
    print(f"{board.name} | nr of animals: {int(nr_animals):.2e}")

    memory_per_s_fpga = memory_per_s(board.freq, lanes, animal_size, cell_size)
    # TODO: Somehow display what the cycle time is
    # print(
    #     f"min cycle time: {cycle_period[0] * 1e3:.2f} ms, "
    #     f"max cycle time {cycle_period[-1] * 1e3:.2f} ms"
    # )

    # TODO: Plot for different number of HALO step sizes
    mem_in_halo = animal_size * memory_for_halo(
        nr_animals, animal_size, cell_size, halo_steps, 6
    )
    memory_on_host = []

    for i, halo_step in enumerate(halo_steps):
        cycle_period = (board.local_memory - mem_in_halo[i]) / memory_per_s_fpga

        memory_on_host.append(board.local_memory * (halo_step + 1) * cycle_period)

    return (memory_on_host, mem_in_halo)


def show_effective_mem_available_on_host(lanes):

    _, ax = plt.subplots(1, 2)
    (memory_on_host, mem_in_halo) = effective_mem_available_on_host(
        COMET_065B, lanes, HALO_STEPS
    )

    max_memory_on_host = 0
    for i, element in enumerate(memory_on_host):
        ax[0].plot(lanes, element, label=f"Halo steps: {HALO_STEPS[i]}")

        max_memory_on_host = max(max_memory_on_host, np.max(memory_on_host))

    ax[1].plot(HALO_STEPS, mem_in_halo)

    ax[0].set_title("Effective free memory on host")
    ax[0].set_xlim(MIN_LANES, MAX_LANES)
    ax[0].set_ylim(0, max_memory_on_host)
    ax[0].set_xlabel("Lanes")
    ax[0].set_ylabel("Max usable RAM on host (Gb)")
    ax[0].yaxis.set_major_formatter(FuncFormatter(bits_to_gb))
    ax[0].legend()

    ax[1].set_xlim(min(HALO_STEPS), max(HALO_STEPS))
    ax[1].set_ylim(0, max(mem_in_halo))
    ax[1].set_title("Memory used for animals in halo")
    ax[1].set_xlabel("Halo steps")
    ax[1].set_ylabel("Memory used for animals in halo (Kb)")
    ax[1].yaxis.set_major_formatter(
        FuncFormatter(lambda x, pos: f"{int(x / (8 * 1e3)):d}")
    )
    plt.show()


def show_compare_boards(lanes):

    boards = [COMET_065B, DE10, HBM2E_STARTER, MERCURY_A2700, AGILEX7_STARTER]

    max_memory_on_host = 0
    _, ax = plt.subplots(1, 2)
    for board in boards:
        (memory_on_host, _) = effective_mem_available_on_host(
            board, lanes, np.asarray([100.0])
        )
        ax[0].plot(lanes, memory_on_host[0], label=board.name)
        max_memory_on_host = max(max_memory_on_host, np.max(memory_on_host[0]))

        ax[1].plot(lanes, memory_on_host[0] / board.local_memory, label=board.name)

    ax[0].set_title("Effective free memory on host")
    ax[0].set_xlim(MIN_LANES, MAX_LANES)
    ax[0].set_ylim(0, max_memory_on_host)
    ax[0].set_xlabel("Lanes")
    ax[0].set_ylabel("Max usable RAM on host (Gb)")
    ax[0].yaxis.set_major_formatter(FuncFormatter(bits_to_gb))
    ax[0].legend()

    ax[1].set_title("Free memory on host vs dev-board")
    ax[1].set_xlim(MIN_LANES, MAX_LANES)
    ax[1].set_ylim(0, 2)
    ax[1].set_xlabel("Lanes")
    ax[1].set_ylabel("Max usable RAM on host (Gb)")
    ax[1].legend()

    plt.show()


def main():

    parser = argparse.ArgumentParser(
        description="Calculate RAM bottleneck to see if RAM on host PC might help"
    )
    parser.add_argument(
        "plot",
        help=(
            "Plot to show: Throughput (FPGA memory throughput vs animal size), "
            "Effective (Usable host memory without necessitating waiting), or Compare "
            "(compare dev-boards)"
        ),
    )
    args = parser.parse_args()

    lanes = np.linspace(MIN_LANES, MAX_LANES)
    animal_sizes = 2 ** np.arange(5, 9)

    match args.plot:
        case "Throughput":
            show_fpga_througput(lanes, animal_sizes)
        case "Effective":
            show_effective_mem_available_on_host(lanes)
        case "Compare":
            show_compare_boards(lanes)


def bits_to_gb(x, pos):
    return f"{x / (8 * 1e9):.2f}"


if __name__ == "__main__":
    main()
