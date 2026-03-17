# imports we will probably need...
from psychopy import visual, event, monitors

import numpy as np

import random, os, sys, time, json, math

import pandas as pd

from serial import Serial

def runExperiment(ID=None):

    # every function, except this one, will get a cfg dictionary as input
    # it will change it, and then also return it as output

    # here we set it up:

    cfg = {}

    # there will be 2 parts of this cfg dictionary:
    # 1. a part that is a more or less "plain text" dictionary,
    #    with objects that can be stored in a JSON
    #    this will be called "state"
    # 2. a part that has binary objects that can not be stored in a JSON
    #    most notably the psychopy window, and stimuearlylus objects
    #    this will be called "bin"

    # after every trial, we store the state, and we also do this in case of exceptions
    # eventually, we'll write a function that can restart the experiment where it 
    # crashed, if that should ever happen, and for this we use the last 'state' that was saved

    cfg['state'] = {}
    cfg['bin']   = {}

    cfg['state']['ID'] = ID

    # everything else we do will live in a try / except / else / finally block:

    try:

        cfg = prepare(cfg)

        # cfg = runFamiliarization(cfg)

        cfg = runTasks(cfg)

    except Exception as e:

        # something went wrong, save the state part of the cfg:

        print(e)

        cfg = saveState(cfg)

        # close the psychopy environment (if still possible)

    else:

        # experiment finished, do final steps:

        # we combine all trial data into a full description
        cfg = combineData(cfg)

        # and maybe generate a summary with one line per trial?
        cfg = createSummary(cfg)

    finally:

        # still also need to close the environment:
        print (cfg)
        cfg = closeEnvironment(cfg)



def prepare(cfg):

    cfg = setupParticipant(cfg)

    cfg = setupEnvironment(cfg)

    cfg = setupTasks(cfg)

    print("preparation finished")

    cfg = saveState(cfg)

    return(cfg)

def setupParticipant(cfg):

    if cfg['state']['ID'] == None:
        print("participant ID not defined")
        raise Exception("participant ID not defined")

    dataFolder = "data/%s/"%(cfg['state']['ID'])

    os.makedirs("data", exist_ok = True)
    if os.path.isdir(dataFolder):
        # participant folder exists
        cfg["state"]["crashRecovery"] = False
        if os.path.isfile("%sstate.json"%(dataFolder)):
            cfg["state"]["crashRecovery"] = True
    else:
        os.makedirs(dataFolder, exist_ok = True)
    cfg["state"]["dataFolder"] = dataFolder

    random.seed(cfg['state']['ID'])  

    # check if participant directory already exists

    # if not, create

    # if it does exist, check if there is a state.json file

    # if there is, check if it finished the experiment, or not
    # (this means we should probably store that the state)

    # if it's not finished, set a toggle for setupTasks, to restore the tasks from the json

    # the environment will still need to be created, because it's not store in the json


    # the other thing that depends on the participant ID is the state of the random-number generator
    # we know the initial state of the random number generator, because we set it with a seed depending on the participant ID

    # what we would not know is the state of the random number generator after it has done stuff, so..
    # if there is an `rngstate` in the state.json, we can `random.setstate(cfg['state']['rngstate'])`
    # and have that right where we left it, so that the experiment will continue exactly as if it hadn't crashed

    # if it's not there, we set it as best as we can

    return(cfg)


def setupEnvironment(cfg):


    # here we do a few things, that each deserve their own function as well
    # all of this will end up in the cfg['bin']

    cfg = setupPsychopyWindow(cfg)

    cfg = setupTabletTracker(cfg)

    cfg = setupStimuli(cfg)

    cfg = setupTVFU(cfg)

    return(cfg)

def setupPsychopyWindow(cfg):

    # this should be a psychopy window object that has the units as centimeters
    myMonitor = monitors.Monitor(name='temp',
                                 distance=100,
                                 width=52.7)
    myMonitor.setSizePix([1920,1080])

    cfg["bin"]['win'] = visual.Window( size=[1920,1080], 
                                fullscr=True, 
                                units='cm', 
                                waitBlanking=False, 
                                #viewScale=[1,-1], 
                                color=[-1,-1,-1], 
                                screen=0, 
                                monitor=myMonitor)

    #   "size_px"         : [1920, 1080], 
    #   "size_cm"         : [52.7, 29.6],
    #   "viewscale"       : [1,-1],

    # for testing on non-mirrored setup:
    #cfg['win'] = visual.Window(fullscr=True, units='pix', waitBlanking=True, viewScale=[1,1], color=[-1,-1,-1])

    # set up the workspace as a function of the size of the window:
    # winSize = cfg['win'].size

    # we want 8 cm reaches
    # if we apply the viewscale correctly, that should be possible
    # leaving 3.375/2 cm free on top and bottom
    # the monitor on the tablet setup is 1920 pixels wide,
    # and that should span 31 cm on the tablet surface

    # cfg['xPPC'] = 1920/52.7
    # cfg['yPPC'] = 1080/29.6

    return(cfg)
    
 # set up 'mouse' object to track reaches:
class myMouse:

    # TABLET:
    # "size_px"    : [1920, 1080],criterion, value
    # "size_cm"    : [31.1, 21.6],
    # "mapping"    : 'relative',     <-   this is not true right now

    # MONITOR:
    #   "size_px"         : [1920, 1080], 
    #   "size_cm"         : [52.7, 29.6],
    #   "viewscale"       : [1,-1],

    def __init__(self,cfg):
        # we use a psychopy mouse object
        self.psyMouse = event.Mouse(visible = False, newPos = None, win = cfg['bin']['win'])
        self.xfactor = 52.7/31.1
        self.yfactor = 29.6/21.6

    def getPos(self):
        # but in addition to the position, we also return the time the position was asked for
        [X,Y] = self.psyMouse.getPos()
        st = time.time()
        X = X / self.xfactor # scale to centimeters ?
        Y = Y / self.yfactor # scale to centimeters ?

        return [X,Y,st]

def setupTabletTracker(cfg):

    # create a custom getpos function
    # using the mouse.getPos thing internally    # http://code.activestate.com/recipes/496807-list-of-all-combination-from-multiple-lists/
    
    cfg['bin']["tracker"] = myMouse(cfg)

    return(cfg)

def setupTVFU(cfg):

    # this is assumed to be the Arduino-based Tactile Vibration Feedback Unit
    # running on our modified / extended version of the Arduino sketch
    # that allows setting intensity and duration as any allowed value

    cfg['bin']['TVFU'] = Serial( port     = '/dev/ttyACM0',
                                 baudrate = 115200          )

    return(cfg)

def setupStimuli(cfg):

    # this creates the stimulu we'll use, for now there are at least 5:

    # 1. a cursor (circle)
    cfg['bin']['cursor'] = visual.Circle(  win=cfg['bin']['win'], 
                                    radius=0.25, 
                                    lineWidth=0, 
                                    lineColorSpace='rgb', 
                                    lineColor=None, 
                                    fillColorSpace='rgb', 
                                    fillColor='#990000'     )
    # 2. a start postition (circle)
    cfg['bin'][''] = visual.Circle(  win=cfg['bin']['win'], 
                                    radius=0.5, 
                                    lineWidth=2, 
                                    lineColorSpace='rgb', 
                                    lineColor='#999999', 
                                    fillColorSpace='rgb', 
                                    fillColor=None          )
    # 3. a target position (circle)
    cfg['bin']['target'] = visual.Circle(  win=cfg['bin']['win'], 
                                    radius=1, 
                                    lineWidth=2, 
                                    lineColorSpace='rgb', 
                                    lineColor='#999999', 
                                    fillColorSpace='rgb', 
                                    fillColor=None          )
    # 4 a prompt to respond to stimulation (circle)
    cfg['bin']['prompt'] = visual.Circle(  win=cfg['bin']['win'], 
                                    radius=1, 
                                    lineWidth=2, 
                                    lineColorSpace='rgb', 
                                    lineColor='#FFFF00', 
                                    fillColorSpace='rgb', 
                                    fillColor=None          )
    # 5. an empty text box for on-screen instructions
    cfg['bin']['instruction'] = visual.TextStim(win=cfg['bin']['win'], text='hello world', pos=[0,0], colorSpace='rgb', color='#999999')


    # these are all psychopy stim objects, associated with the window we created


    return(cfg)



def foldout(values, names):
    # http://code.activestate.com/recipes/496807-list-of-all-combination-from-multiple-lists/



    r=[[]]
    for x in values:
        r = [ i + [y] for y in x for i in r ]

    # return(pd.DataFrame(r))

    df = pd.DataFrame(r)
    df.columns = names
    return(df.to_dict())

def setupTasks(cfg):

    leftTargets = [(-10, 5), (-10, -5)]
    rightTargets = [(10, 5), (10, -5)]

    cfg['state']['leftTargets'] = leftTargets
    cfg['state']['rightTargets'] = rightTargets

    leftPrompt = [(-10, 5), (-10, -5)]
    rightPrompt = [(10, 5), (10, -5)]

    cfg['state']['leftPrompt'] = leftPrompt
    cfg['state']['rightPrompt'] = rightPrompt

    # master list of all conditions, positon, size
    leftConditions = foldout(values = [[1, 0.5],  
                                    leftTargets, leftPrompt],
                         names = ["targetSize", "targetPos", "promptPos"])
    rightConditions = foldout(values = [[1, 0.5],
                                    rightTargets, rightPrompt],
                         names = ["targetSize", "targetPos", "promptPos"])
    conditions = pd.concat([pd.DataFrame(leftConditions), pd.DataFrame(rightConditions)], ignore_index = True).to_dict()
    cfg["state"]["conditions"] = conditions

    # create a randomized list of trials unique to each participant ID

    Nconditions = len(conditions[list(conditions.keys())[0]])
    CondIdxOne = list(range(int(Nconditions/2)))
    CondIdxTwo = list(range(int(Nconditions/2), Nconditions))
    if random.sample([True, False],1):
        CondIdxOne, CondIdxTwo = CondIdxTwo, CondIdxOne

    # two pseudo-randomized blocks

    trialOrder = []
    for blockNo in range(16):
        random.shuffle(CondIdxOne)
        random.shuffle(CondIdxTwo)
        for Idx in range(len(CondIdxOne)):
            trialOrder.append(CondIdxOne[Idx])
            trialOrder.append(CondIdxTwo[Idx])

    cfg["state"]["trialOrder"] = trialOrder
    cfg['state']['response']   = {'trial'      : [],
                                  'response'   : [],
                                 }

    return(cfg)

def runFamiliarization(cfg):

    # cfg['bin']['instruction'] = visual.TextStim(win=cfg['bin']['win'], text='hello world', pos=[0,0], colorSpace='rgb', color='#999999')
    # cfg['bin']['instruction'].text = 'On each trial, you will see a red dot. If you detect a vibration, press the RIGHT ARROW key when the dot turns blue. If you do not detect a vibration, press the LEFT ARROW key. When you are ready, press SPACE to start the familiarization task.'
    cfg['bin']['instruction'].text = 'On each trial, you will see a red dot. When a target appears, use the stylus to move the dot inside the target. Wait for the target to turn yellow, then press the RIGHT arrow key to start the next trial. When you are ready, press SPACE to start the familiarization task.'
    cfg['bin']['instruction'].draw()
    cfg['bin']['win'].flip()

    # wait for press of the space bar:
    k = ['wait']
    while k[0] not in ['space', 'q']:
        k = event.waitKeys()
        # cfg['bin']['instruction'].draw()
        # cfg['bin']['win'].flip()

    if k[0] in ['q']:
        cfg['ext']['quit'] = True

    strengths = [0,0,0,0,127,127,127,127]
    random.shuffle(strengths)

    motor = 2
    duration = 200

    for strength in strengths:

        prepcommand = 'M%d.S%d.D%d.'%(motor, strength, duration)

        cfg['bin']['TVFU'].write(bytes(prepcommand, 'utf-8'))

        # blank screen
        blank = 0.5
        # stimulus cue
        # wait interval
        # stimulus jitter
        jitter = random.uniform(0, 1)
        # stimulus presentation
        stimulusOnset = blank + jitter
        # response cue
        cueTime = blank + 1.5
        # get response
        # store data and end trial
        startTime = time.time()

        presentStimulus = True
        inStimulusInterval = True
        cfg['bin']['cursor'].fillColor = '#ff0000'


        while inStimulusInterval:
            now = time.time()
            if now > (startTime + stimulusOnset):
                # do tactile stimulation
                    if (presentStimulus):
                        presentStimulus = False
                        cfg['bin']['TVFU'].write(bytes('G2.', 'utf-8'))

                    pass
            if now > (startTime + cueTime):
                cfg['bin']['cursor'].fillColor = '#0000ff'
                inStimulusInterval = False
            if now > (startTime + blank):
                cfg['bin']['cursor'].draw()
            cfg['bin']['win'].flip()

        # wait for response
            k = ['wait']

        # left  = no stimulus detected  -> go up the staircase
        # right = detected stimulus     -> go down the staircase

        correct = True

        while k[0] not in ['left', 'right', 'q']:
            k = event.waitKeys()
        if k[0] in ['q']:
            # quit task?
            response = -1
            print('Q pressed')
            cfg['ext']['quit'] = True
        if k[0] in ['left']:
            response = 1
            if strength > 0:
                correct = False
        if k[0] in ['right']:
            response = 2
            if strength == 0:
                correct = False


        # if correct:
        #     cfg['bin']['instruction'].text = 'correct'
        # else:
            #     cfg['bin']['instruction'].text = 'incorrect'

        cfg['bin']['instruction'].text = 'correct' if correct else 'incorrect'

        cfg['bin']['instruction'].draw()
        cfg['bin']['win'].flip()

        startTime = time.time()
        now = time.time()
        while now < (startTime + 1.0):
            cfg['bin']['instruction'].draw()
            cfg['bin']['win'].flip()
            now = time.time()

    return(cfg)

def runTasks(cfg):
    print("runTasks")

    # cfg['bin']['instruction'].text = 'When the target appears, move the cursor inside the target. Once inside the target, if you detected a vibration, press the RIGHT ARROW key. If you did not detect a vibration, press the LEFT ARROW key. After you have responded, move the cursor inside the start position to begin the next trial. Press SPACE to begin the task.'
    cfg['bin']['instruction'].text = 'On each trial, you will see a red dot. When a target appears, use the stylus to move the dot inside the target. Wait for the target to turn yellow, then press the RIGHT arrow key to start the next trial. Move or keep the cursor inside the new start position until the next target appears. When you are ready, press SPACE to start the task.'
    cfg['bin']['instruction'].draw()
    cfg['bin']['win'].flip()
    cfg['bin']['cursor'].fillColor = '#ff0000'

        # wait for press of the space bar:
    k = ['wait']
    while k[0] not in ['space', 'q']:
        k = event.waitKeys()
        # cfg['bin']['instruction'].draw()
        # cfg['bin']['win'].flip()

    if k[0] in ['q']:
        cfg['ext']['quit'] = True

    print(len(cfg["state"]["trialOrder"]))
    trialOrder = cfg['state']['trialOrder']
    nTrials = len(trialOrder)
    for trialNumber in range(nTrials):
        print(trialNumber)
        cfg['state']['trialNumber'] = trialNumber

        # show instructions if applicable

        cfg = runTrial(cfg)
       #  cfg = saveState(cfg)

    return(cfg)

def runTrial(cfg):

    # cfg['ext']['quit'] = False
    # if k[0] in ['q']:
    #    print('Q pressed')
    #    cfg['ext']['quit'] = True

    trialNumber =  cfg['state']['trialNumber']
    print(trialNumber)

    condIdx = cfg['state']['trialOrder'][trialNumber]
    targetSize = cfg['state']['conditions']['targetSize'][condIdx]
    targetPos = cfg['state']['conditions']['targetPos'][condIdx]
    promptPos = cfg['state']['conditions']['targetPos'][condIdx]
    print(targetPos)
    print(promptPos)
    print("getting start pos")
    if trialNumber > 0:
        print("get startPos from previous trial")
        prevCondIdx = cfg['state']['trialOrder'][trialNumber-1]
        startPos = cfg['state']['conditions']['targetPos'][prevCondIdx]
        promptPos = cfg['state']['conditions']['targetPos'][condIdx]
        if trialNumber in [73, 145, 217] :
            cfg['bin']['instruction'].text = 'Take a break! When you are ready, press SPACE to continue.'
            cfg['bin']['instruction'].draw()
            cfg['bin']['win'].flip()
            cfg['bin']['cursor'].fillColor = '#ff0000'

        # wait for press of the space bar:
            k = ['wait']
            while k[0] not in ['space', 'q']:
                k = event.waitKeys()

        # cfg['bin']['instruction'].draw()
        # cfg['bin']['win'].flip()

            if k[0] in ['q']:
                cfg['ext']['quit'] = True
    else:
        # startPos = [0, 0] 
        if tuple(targetPos) in cfg['state']['leftTargets']:

            # randomly pick a right Target

            startPos = random.sample(cfg['state']['rightTargets'], 1)[0]

        else:

            # randomly pick a left Target

            startPos = random.sample(cfg['state']['leftTargets'], 1)[0]
    print("assigning positions")
    cfg['bin']['target'].pos = targetPos
    cfg['bin']['prompt'].pos = promptPos
    cfg['bin']['target'].radius = targetSize
    cfg['bin']['prompt'].radius = targetSize
    cfg['bin']['start'].pos = startPos
    print("make rotation matrix")
    print(targetPos)
    print(startPos)
    theta = -1 * np.arctan2(targetPos[1] - startPos[1],targetPos[0] - startPos[0])
    R = np.array([[np.cos(theta),-1*np.sin(theta)],[np.sin(theta),np.cos(theta)]],order='C')

    runningTrial = True

    phase = 0

    # create lists to store data in:
    stylusX = []
    stylusY = []
    cursorX = []
    cursorY = []
    time_s = []
    phases = []

    print("start while loop")
    while runningTrial:

        [x,y,t] = cfg['bin']["tracker"].getPos()
        cursorPos = [x,y]
        cfg['bin']['cursor'].pos = cursorPos

        stylusX.append(x)
        stylusY.append(y)
        cursorX.append(x)
        cursorY.append(y)
        time_s.append(t)
        phases.append(phase)

        # end trial
        if phase == 9:
            runningTrial = False  

        # after the prompt, the participant needs to respond if they felt stimulation
        if phase == 8:
            cfg['bin']['prompt'].draw()
            cfg['bin']['cursor'].draw()
            k = ['wait']
            correct = True
            # check if hold-distance is maintained
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance > cfg['bin']['target'].radius:
                phase = 98
            # if distance < cfg['bin']['target'].radius:
            #    phase = 5
            while k[0] not in ['left', 'right', 'q']:
                k = event.waitKeys()
                if k[0] in ['q']:
                    # quit task?
                    response = -1
                    print('Q pressed')
                    cfg['ext']['quit'] = True
                if k[0] in ['left']:
                    response = 1
                    # if strength > 0:
                    #    correct = False
                if k[0] in ['right']:
                    response = 2
                    # if strength == 0:                    #    correct = False
            if k[0] in ['left','right']:
                prompt_RT = time.time() - promptTime
                phase = 9

        # after holding, draw the prompt to respond
        if phase == 7:
            cfg['bin']['prompt'].draw()
            cfg['bin']['cursor'].draw()
            promptTime = time.time()
            # check if hold-distance is maintained
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance > cfg['bin']['target'].radius:
                phase = 98
            if distance < cfg['bin']['target'].radius:
                phase = 8
    
        # after returning to the target, the participant needs to hold
        if phase == 99:
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            # check if hold-distance is maintained
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance > cfg['bin']['target'].radius:
                phase = 98
            # check if hold-time is complete
            if (time.time() > (holdStartTime + 0.5)):
                phase = 7

        # participant needs to return to the target
        if phase == 98:
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            #check if target has been reached
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance < cfg['bin']['target'].radius:
                holdStartTime = time.time()
                phase = 99 
        
        # after reaching the target, the participant needs to hold
        if phase == 6:
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            # check if hold-distance is maintained
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance > cfg['bin']['target'].radius:
                phase = 98
            # check if hold-time is complete
            if (time.time() > (holdStartTime + 0.5)):
                phase = 7

        # participant moves to target
        # check if target has been reached
        if phase == 5:
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
            if distance < cfg['bin']['target'].radius:
                movementEndTime = time.time()
                movement_time = (movementEndTime - movementStartTime)
                holdStartTime = time.time()
                enterTargetPosX = cursorPos[0]
                enterTargetPosY = cursorPos[1]
                startToTarget_time = (time.time() - leaveStartPositionTime)
                phase = 6

        # participant needs to move to the target
        # stimulation is delivered
        # check that participant has left start position
        if phase == 4:
            cfg['bin']['start'].draw()
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            # now = time.time()
            # if now > (startTime + stimulusOnset):
            #     # do tactile stimulation
            #         if (presentStimulus):
            #             presentStimulus = False
            #             cfg['bin']['TVFU'].write(bytes('G2.', 'utf-8'))
            # check if cursor has left start position
            distance = ((cursorPos[0]-startPos[0])**2+(cursorPos[1]-startPos[1])**2)**0.5
            if distance > cfg['bin']['start'].radius:
                leaveStartPositionTime = time.time()
                target_RT = (time.time() - targetOnsetTime)
                phase = 5

        # check if cursor has moved
        if phase == 3:
            cfg['bin']['start'].draw()
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            # now = time.time()
            # if now > (startTime + stimulusOnset):
            #     # do tactile stimulation
            #         if (presentStimulus):
            #             presentStimulus = False
            #             cfg['bin']['TVFU'].write(bytes('G2.', 'utf-8'))
            # check if cursor has moved
            if cursorPos != cursorStartPos:
                movementStartTime = time.time()
                phase = 4

        # draw target and check that cursor is still in start positon
        if phase == 2:
            cfg['bin']['start'].draw()
            cfg['bin']['target'].draw()
            cfg['bin']['cursor'].draw()
            targetOnsetTime = time.time()
            # check if cursor is still in start position
            distance = ((cursorPos[0]-startPos[0])**2+(cursorPos[1]-startPos[1])**2)**0.5
            if distance < cfg['bin']['start'].radius:
                phase = 3

        # participant must hold in the start position
        if phase ==1:
            cfg['bin']['start'].draw()
            cfg['bin']['cursor'].draw()
            distance = ((cursorPos[0]-startPos[0])**2+(cursorPos[1]-startPos[1])**2)**0.5
            if distance < cfg['bin']['start'].radius:
                if (time.time() > (startStartTime + 0.5)):
                    cursorStartPos = cursorPos
                    phase = 2

        # draw start position and cursor
        # participant must move to start position
        if phase == 0:
            trialStart = time.time()
            cfg['bin']['start'].draw()
            cfg['bin']['cursor'].draw()
            # check if cursor is in the home position
            distance = ((cursorPos[0]-startPos[0])**2+(cursorPos[1]-startPos[1])**2)**0.5
            if distance < cfg['bin']['start'].radius:
                startStartTime = time.time()
                phase = 1

        # distance = ((cursorPos[0]-targetPos[0])**2+(cursorPos[1]-targetPos[1])**2)**0.5
        # if distance < 0.5:
        #     runningTrial = False
        # cfg['bin']['target'].draw()
        # cfg['bin']['start'].draw()
        # cfg['bin']['cursor'].draw()
        cfg['bin']['win'].flip()

        # add data sample for the current frame:

    
    # collect all data in matrix/dictionary?
    nsamples = len(time_s)

    trial_idx = [trialNumber+1] * nsamples
    targetx = [targetPos[0]] * nsamples
    targety = [targetPos[1]] * nsamples
    startx = [startPos[0]] * nsamples
    starty = [startPos[1]] * nsamples

    # put all lists in dictionary:
    trialdata = { 'trial_idx'           : trial_idx,
                  'startx_cm'           : startx,
                  'starty_cm'           : starty,
                  'targetx_cm'          : targetx,
                  'targety_cm'          : targety,
                  'targetSize'          : targetSize,
                  'phase'               : phases,      
                  'time_s'              : time_s,
                  'stylusx_cm'          : stylusX,
                  'stylusy_cm'          : stylusY,
                  'cursorx_cm'          : cursorX,
                  'cursory_cm'          : cursorY,
                  'target_RT'           : target_RT,
                  'startToTarget_time'  : startToTarget_time,
                  'movement_time'       : movement_time,
                  'response'            : response,
                  'prompt_RT'           : prompt_RT,
                  'enterTargetPosX'     : enterTargetPosX,
                  'enterTargetPosY'     : enterTargetPosY,
                  }

    # make dictionary into data frame:
    trialdata = pd.DataFrame(trialdata)

    # calculate distance from start position to target position
    trialdata['distance'] = np.sqrt(((trialdata['startx_cm'] - trialdata['targetx_cm']) ** 2) + ((trialdata['starty_cm'] - trialdata['targety_cm']) ** 2))

    # calculate precision error
    trialdata['ballistic_x_error'] = trialdata['enterTargetPosX'] - trialdata['targetx_cm']
    trialdata['ballistic_y_error'] = trialdata['enterTargetPosY'] - trialdata['targety_cm']
    trialdata['ballistic_ABS_error'] = (trialdata['ballistic_x_error']**2 + trialdata['ballistic_y_error']**2)**0.5
    trialdata['absolute_ballistic_x_error'] = abs(trialdata['ballistic_x_error'])
    trialdata['absolute_ballistic_y_error'] = abs(trialdata['ballistic_y_error'])
    trialdata['absolute_ballistic_ABS_error'] = abs((trialdata['ballistic_x_error']**2 + trialdata['ballistic_y_error']**2)**0.5)
    trialdata['endpoint_x_error'] = trialdata['cursorx_cm'] - trialdata['targetx_cm']
    trialdata['endpoint_y_error'] = trialdata['cursory_cm'] - trialdata['targety_cm']
    trialdata['endpoint_ABS_error'] = (trialdata['endpoint_x_error']**2 + trialdata['endpoint_y_error']**2)**0.5
    trialdata['absolute_endpoint_x_error'] = abs(trialdata['cursorx_cm'] - trialdata['targetx_cm'])
    trialdata['absolute_endpoint_y_error'] = abs(trialdata['cursory_cm'] - trialdata['targety_cm'])
    trialdata['absolute_endpoint_ABS_error'] = abs((trialdata['endpoint_x_error']**2 + trialdata['endpoint_y_error']**2)**0.5)

    # convert Linux time to trial time:
    trialdata['trialtime'] = trialdata['time_s'] - trialdata['time_s'].iloc[0]

    # calculate angle between start position and target postion, rotate, then calculate max cursor pos deviation from y = 0
    # target = [targetx, targety]
    # start = [startx, starty]
    # y = target[1]-start[1]
    # x = target[0]-start[0]
    # theta = np.arctan2((y, x)/np.pi*180)*(-1)
    
    # store data frame:
    filename = 'data/%s/trial%04d.csv'%(cfg['state']['ID'],cfg['state']['trialNumber']+1)
    trialdata.to_csv( filename, index=False, float_format='%0.5f' )

    # df = pd.read_csv('data/%s/trial%04d.csv'%(cfg['state']['ID'],cfg['state']['trialNumber']+1))
    # df['trialTime'] = df['time_s'] - df['time_s'].iloc[0] / 1000

    return(cfg)

def saveState(cfg):

    # generate a file name, open a stream, and save state as JSON into the stream
    
    filename = '%sstate.json'%(cfg["state"]["dataFolder"])

    with open(file=filename,
               mode='w') as fp:
        json.dump(cfg["state"], fp, indent=2)

    return(cfg)

def combineData(cfg):

    # combine all the data, store trial data in a list of dataframes:
    trialdataframes = []

    # loop through tasks:
    for taskno in list(range(len(cfg['tasks']))):

        task = cfg['tasks'][taskno]

        # loop through trials:
        for trialno in list(range(len(task['target']))):

            # load trial data and store in lists:
            filename = 'data/%s/trial%04d.csv'%(cfg["state"]["dataFolder"]+1)
            trialdata.to_csv( filename, index=False, float_format='%0.5f' )

    # concatenate all data frames
    combineData = pd.concat(trialdataframes)

    # store combined data in one file
    filename = 'data/%s/COMBINED_%s.csv'%(cfg["state"]["dataFolder"])
    combinedData.to_csv( filename, index = False, float_format='%0.5f' )

    return(cfg)

def closeEnvironment(cfg):

    cfg['bin']['win'].close()

    return(cfg)
