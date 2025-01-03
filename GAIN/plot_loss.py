"""
Plot output from GAIN model training
Author: Chayut Wongkamthong 05/08/2020
"""
import matplotlib.pyplot as plt

def plot_loss(generator_loss, discriminator_loss, mse_loss):
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
    #plt.ylim(0,2)
    plt.title('GAIN Learning progression: Generator')
    fig.savefig('./plots/gain_generator.png')

    # plot loss over iterations for discriminator
    fig = plt.figure()
    plt.plot(discriminator_loss, label='discriminator loss')
    plt.xlabel('iteration')
    plt.ylabel('discriminator loss')
    #plt.ylim(0,2)
    plt.title('GAIN Learning progression: Discriminator')
    fig.savefig('./plots/gain_discriminator.png')

    # plot loss over iterations for mse loss
    fig = plt.figure()
    plt.plot(mse_loss, label='MSE loss')
    plt.xlabel('iteration')
    plt.ylabel('MSE loss')
    #plt.ylim(0,0.1)
    plt.title('GAIN Learning progression: MSE loss')
    fig.savefig('./plots/gain_mse.png')
    return
