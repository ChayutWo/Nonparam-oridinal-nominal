"""
Plot output from GAIN model training
Author: Chayut Wongkamthong 05/08/2020
"""
import matplotlib.pyplot as plt

def plot_loss(generator_loss, discriminator_loss):
    """
    plot loss over training process and save figure
    Args:
        generator_loss: a list containing generator loss over the training process
        discriminator_loss: a list containing discriminator loss over the training process

    Returns: None

    """
    # plot loss over iterations for generator
    plt.rcParams['figure.figsize'] = [10,6]
    plt.rcParams.update({'font.size':14})
    fig = plt.figure()
    plt.plot(generator_loss, label = 'generator loss')
    plt.xlabel('iteration')
    plt.ylabel('generator loss')
    plt.ylim(bottom = 0)
    plt.title('GAIN Learning progression: Generator')
    fig.savefig('./plots/gain_generator.png')

    # plot loss over iterations for discriminator
    fig = plt.figure()
    plt.plot(discriminator_loss, label='discriminator loss')
    plt.xlabel('iteration')
    plt.ylabel('discriminator loss')
    plt.ylim(bottom = 0)
    plt.title('GAIN Learning progression: Discriminator')
    fig.savefig('./plots/gain_discriminator.png')
    return
