FROM tiagopeixoto/graph-tool

# Modify the default user (creatively named 'user') to have the same UID as the person building image
# Add a new user with desired permissions
ARG UID=1000

# Make a new user so we can give custom UID
RUN useradd -ms /bin/bash analysis

# Set userid to some crazy value so we can modify it even if it's already what we need
RUN echo "Building user id is $UID"

# Now set userid to match the user id of the builder of the image
RUN usermod -u $UID analysis

USER analysis
WORKDIR /home/analysis

# Copy the neccesary scripts into the docker container
COPY run_bisbm.py /home/analysis/run_bisbm.py

ENTRYPOINT python run_bisbm.py